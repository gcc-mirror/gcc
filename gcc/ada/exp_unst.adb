------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ U N S T                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2014-2018, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Atree;    use Atree;
with Debug;    use Debug;
with Einfo;    use Einfo;
with Elists;   use Elists;
with Lib;      use Lib;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Opt;
with Output;   use Output;
with Rtsfind;  use Rtsfind;
with Sem;      use Sem;
with Sem_Aux;  use Sem_Aux;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Mech; use Sem_Mech;
with Sem_Res;  use Sem_Res;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Sinput;   use Sinput;
with Snames;   use Snames;
with Stand;    use Stand;
with Tbuild;   use Tbuild;
with Uintp;    use Uintp;

package body Exp_Unst is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Unnest_Subprogram (Subp : Entity_Id; Subp_Body : Node_Id);
   --  Subp is a library-level subprogram which has nested subprograms, and
   --  Subp_Body is the corresponding N_Subprogram_Body node. This procedure
   --  declares the AREC types and objects, adds assignments to the AREC record
   --  as required, defines the xxxPTR types for uplevel referenced objects,
   --  adds the ARECP parameter to all nested subprograms which need it, and
   --  modifies all uplevel references appropriately.

   -----------
   -- Calls --
   -----------

   --  Table to record calls within the nest being analyzed. These are the
   --  calls which may need to have an AREC actual added. This table is built
   --  new for each subprogram nest and cleared at the end of processing each
   --  subprogram nest.

   type Call_Entry is record
      N : Node_Id;
      --  The actual call

      Caller : Entity_Id;
      --  Entity of the subprogram containing the call (can be at any level)

      Callee : Entity_Id;
      --  Entity of the subprogram called (always at level 2 or higher). Note
      --  that in accordance with the basic rules of nesting, the level of To
      --  is either less than or equal to the level of From, or one greater.
   end record;

   package Calls is new Table.Table (
     Table_Component_Type => Call_Entry,
     Table_Index_Type     => Nat,
     Table_Low_Bound      => 1,
     Table_Initial        => 100,
     Table_Increment      => 200,
     Table_Name           => "Unnest_Calls");
   --  Records each call within the outer subprogram and all nested subprograms
   --  that are to other subprograms nested within the outer subprogram. These
   --  are the calls that may need an additional parameter.

   procedure Append_Unique_Call (Call : Call_Entry);
   --  Append a call entry to the Calls table. A check is made to see if the
   --  table already contains this entry and if so it has no effect.

   ----------------------------------
   -- Subprograms For Fat Pointers --
   ----------------------------------

   function Build_Access_Type_Decl
     (E    : Entity_Id;
      Scop : Entity_Id) return Node_Id;
   --  For an uplevel reference that involves an unconstrained array type,
   --  build an access type declaration for the corresponding activation
   --  record component. The relevant attributes of the access type are
   --  set here to avoid a full analysis that would require a scope stack.

   function Needs_Fat_Pointer (E : Entity_Id) return Boolean;
   --  A formal parameter of an unconstrained array type that appears in an
   --  uplevel reference requires the construction of an access type, to be
   --  used in the corresponding component declaration.

   -----------
   -- Urefs --
   -----------

   --  Table to record explicit uplevel references to objects (variables,
   --  constants, formal parameters). These are the references that will
   --  need rewriting to use the activation table (AREC) pointers. Also
   --  included are implicit and explicit uplevel references to types, but
   --  these do not get rewritten by the front end. This table is built new
   --  for each subprogram nest and cleared at the end of processing each
   --  subprogram nest.

   type Uref_Entry is record
      Ref : Node_Id;
      --  The reference itself. For objects this is always an entity reference
      --  and the referenced entity will have its Is_Uplevel_Referenced_Entity
      --  flag set and will appear in the Uplevel_Referenced_Entities list of
      --  the subprogram declaring this entity.

      Ent : Entity_Id;
      --  The Entity_Id of the uplevel referenced object or type

      Caller : Entity_Id;
      --  The entity for the subprogram immediately containing this entity

      Callee : Entity_Id;
      --  The entity for the subprogram containing the referenced entity. Note
      --  that the level of Callee must be less than the level of Caller, since
      --  this is an uplevel reference.
   end record;

   package Urefs is new Table.Table (
     Table_Component_Type => Uref_Entry,
     Table_Index_Type     => Nat,
     Table_Low_Bound      => 1,
     Table_Initial        => 100,
     Table_Increment      => 200,
     Table_Name           => "Unnest_Urefs");

   ------------------------
   -- Append_Unique_Call --
   ------------------------

   procedure Append_Unique_Call (Call : Call_Entry) is
   begin
      for J in Calls.First .. Calls.Last loop
         if Calls.Table (J) = Call then
            return;
         end if;
      end loop;

      Calls.Append (Call);
   end Append_Unique_Call;

   -----------------------------
   --  Build_Access_Type_Decl --
   -----------------------------

   function Build_Access_Type_Decl
     (E    : Entity_Id;
      Scop : Entity_Id) return Node_Id
   is
      Loc : constant Source_Ptr := Sloc (E);
      Typ : Entity_Id;

   begin
      Typ := Make_Temporary (Loc, 'S');
      Set_Ekind (Typ, E_General_Access_Type);
      Set_Etype (Typ, Typ);
      Set_Scope (Typ, Scop);
      Set_Directly_Designated_Type (Typ, Etype (E));

      return
        Make_Full_Type_Declaration (Loc,
          Defining_Identifier => Typ,
          Type_Definition     =>
            Make_Access_To_Object_Definition (Loc,
              Subtype_Indication => New_Occurrence_Of (Etype (E), Loc)));
   end Build_Access_Type_Decl;

   ---------------
   -- Get_Level --
   ---------------

   function Get_Level (Subp : Entity_Id; Sub : Entity_Id) return Nat is
      Lev : Nat;
      S   : Entity_Id;

   begin
      Lev := 1;
      S   := Sub;
      loop
         if S = Subp then
            return Lev;
         else
            Lev := Lev + 1;
            S   := Enclosing_Subprogram (S);
         end if;
      end loop;
   end Get_Level;

   --------------------------
   -- In_Synchronized_Unit --
   --------------------------

   function In_Synchronized_Unit (Subp : Entity_Id) return Boolean is
      S : Entity_Id := Scope (Subp);

   begin
      while Present (S) and then S /= Standard_Standard loop
         if Is_Concurrent_Type (S) then
            return True;
         end if;

         S := Scope (S);
      end loop;

      return False;
   end In_Synchronized_Unit;

   -----------------------
   -- Needs_Fat_Pointer --
   -----------------------

   function Needs_Fat_Pointer (E : Entity_Id) return Boolean is
   begin
      return Is_Formal (E)
        and then Is_Array_Type (Etype (E))
        and then not Is_Constrained (Etype (E));
   end Needs_Fat_Pointer;

   ----------------
   -- Subp_Index --
   ----------------

   function Subp_Index (Sub : Entity_Id) return SI_Type is
      E : Entity_Id := Sub;

   begin
      pragma Assert (Is_Subprogram (E));

      if Subps_Index (E) = Uint_0 then
         E := Ultimate_Alias (E);

         if Ekind (E) = E_Function
           and then Rewritten_For_C (E)
           and then Present (Corresponding_Procedure (E))
         then
            E := Corresponding_Procedure (E);
         end if;
      end if;

      pragma Assert (Subps_Index (E) /= Uint_0);
      return SI_Type (UI_To_Int (Subps_Index (E)));
   end Subp_Index;

   -----------------------
   -- Unnest_Subprogram --
   -----------------------

   procedure Unnest_Subprogram (Subp : Entity_Id; Subp_Body : Node_Id) is
      function AREC_Name (J : Pos; S : String) return Name_Id;
      --  Returns name for string ARECjS, where j is the decimal value of j

      function Enclosing_Subp (Subp : SI_Type) return SI_Type;
      --  Subp is the index of a subprogram which has a Lev greater than 1.
      --  This function returns the index of the enclosing subprogram which
      --  will have a Lev value one less than this.

      function Img_Pos (N : Pos) return String;
      --  Return image of N without leading blank

      function Upref_Name
        (Ent   : Entity_Id;
         Index : Pos;
         Clist : List_Id) return Name_Id;
      --  This function returns the name to be used in the activation record to
      --  reference the variable uplevel. Clist is the list of components that
      --  have been created in the activation record so far. Normally the name
      --  is just a copy of the Chars field of the entity. The exception is
      --  when the name has already been used, in which case we suffix the name
      --  with the index value Index to avoid duplication. This happens with
      --  declare blocks and generic parameters at least.

      ---------------
      -- AREC_Name --
      ---------------

      function AREC_Name (J : Pos; S : String) return Name_Id is
      begin
         return Name_Find ("AREC" & Img_Pos (J) & S);
      end AREC_Name;

      --------------------
      -- Enclosing_Subp --
      --------------------

      function Enclosing_Subp (Subp : SI_Type) return SI_Type is
         STJ : Subp_Entry renames Subps.Table (Subp);
         Ret : constant SI_Type := Subp_Index (Enclosing_Subprogram (STJ.Ent));
      begin
         pragma Assert (STJ.Lev > 1);
         pragma Assert (Subps.Table (Ret).Lev = STJ.Lev - 1);
         return Ret;
      end Enclosing_Subp;

      -------------
      -- Img_Pos --
      -------------

      function Img_Pos (N : Pos) return String is
         Buf : String (1 .. 20);
         Ptr : Natural;
         NV  : Nat;

      begin
         Ptr := Buf'Last;
         NV := N;
         while NV /= 0 loop
            Buf (Ptr) := Character'Val (48 + NV mod 10);
            Ptr := Ptr - 1;
            NV := NV / 10;
         end loop;

         return Buf (Ptr + 1 .. Buf'Last);
      end Img_Pos;

      ----------------
      -- Upref_Name --
      ----------------

      function Upref_Name
        (Ent   : Entity_Id;
         Index : Pos;
         Clist : List_Id) return Name_Id
      is
         C : Node_Id;
      begin
         C := First (Clist);
         loop
            if No (C) then
               return Chars (Ent);

            elsif Chars (Defining_Identifier (C)) = Chars (Ent) then
               return
                 Name_Find (Get_Name_String (Chars (Ent)) & Img_Pos (Index));
            else
               Next (C);
            end if;
         end loop;
      end Upref_Name;

   --  Start of processing for Unnest_Subprogram

   begin
      --  Nothing to do inside a generic (all processing is for instance)

      if Inside_A_Generic then
         return;
      end if;

      --  If the main unit is a package body then we need to examine the spec
      --  to determine whether the main unit is generic (the scope stack is not
      --  present when this is called on the main unit).

      if Ekind (Cunit_Entity (Main_Unit)) = E_Package_Body
        and then Is_Generic_Unit (Spec_Entity (Cunit_Entity (Main_Unit)))
      then
         return;
      end if;

      --  Only unnest when generating code for the main source unit

      if not In_Extended_Main_Code_Unit (Subp_Body) then
         return;
      end if;

      --  This routine is called late, after the scope stack is gone. The
      --  following creates a suitable dummy scope stack to be used for the
      --  analyze/expand calls made from this routine.

      Push_Scope (Subp);

      --  First step, we must mark all nested subprograms that require a static
      --  link (activation record) because either they contain explicit uplevel
      --  references (as indicated by Is_Uplevel_Referenced_Entity being set at
      --  this point), or they make calls to other subprograms in the same nest
      --  that require a static link (in which case we set this flag).

      --  This is a recursive definition, and to implement this, we have to
      --  build a call graph for the set of nested subprograms, and then go
      --  over this graph to implement recursively the invariant that if a
      --  subprogram has a call to a subprogram requiring a static link, then
      --  the calling subprogram requires a static link.

      --  First populate the above tables

      Subps_First := Subps.Last + 1;
      Calls.Init;
      Urefs.Init;

      Build_Tables : declare
         Current_Subprogram : Entity_Id;
         --  When we scan a subprogram body, we set Current_Subprogram to the
         --  corresponding entity. This gets recursively saved and restored.

         function Visit_Node (N : Node_Id) return Traverse_Result;
         --  Visit a single node in Subp

         -----------
         -- Visit --
         -----------

         procedure Visit is new Traverse_Proc (Visit_Node);
         --  Used to traverse the body of Subp, populating the tables

         ----------------
         -- Visit_Node --
         ----------------

         function Visit_Node (N : Node_Id) return Traverse_Result is
            Ent    : Entity_Id;
            Caller : Entity_Id;
            Callee : Entity_Id;

            procedure Check_Static_Type
              (T : Entity_Id; N : Node_Id; DT : in out Boolean);
            --  Given a type T, checks if it is a static type defined as a type
            --  with no dynamic bounds in sight. If so, the only action is to
            --  set Is_Static_Type True for T. If T is not a static type, then
            --  all types with dynamic bounds associated with T are detected,
            --  and their bounds are marked as uplevel referenced if not at the
            --  library level, and DT is set True. If N is specified, it's the
            --  node that will need to be replaced. If not specified, it means
            --  we can't do a replacement because the bound is implicit.

            procedure Note_Uplevel_Ref
              (E      : Entity_Id;
               N      : Node_Id;
               Caller : Entity_Id;
               Callee : Entity_Id);
            --  Called when we detect an explicit or implicit uplevel reference
            --  from within Caller to entity E declared in Callee. E can be a
            --  an object or a type.

            procedure Register_Subprogram (E : Entity_Id; Bod : Node_Id);
            --  Enter a subprogram whose body is visible or which is a
            --  subprogram instance into the subprogram table.

            -----------------------
            -- Check_Static_Type --
            -----------------------

            procedure Check_Static_Type
              (T : Entity_Id; N : Node_Id; DT : in out Boolean)
            is
               procedure Note_Uplevel_Bound (N : Node_Id; Ref : Node_Id);
               --  N is the bound of a dynamic type. This procedure notes that
               --  this bound is uplevel referenced, it can handle references
               --  to entities (typically _FIRST and _LAST entities), and also
               --  attribute references of the form T'name (name is typically
               --  FIRST or LAST) where T is the uplevel referenced bound.
               --  Ref, if Present, is the location of the reference to
               --  replace.

               ------------------------
               -- Note_Uplevel_Bound --
               ------------------------

               procedure Note_Uplevel_Bound (N : Node_Id; Ref : Node_Id) is
               begin
                  --  Entity name case. Make sure that the entity is declared
                  --  in a subprogram. This may not be the case for for a type
                  --  in a loop appearing in a precondition.
                  --  Exclude explicitly  discriminants (that can appear
                  --  in bounds of discriminated components).

                  if Is_Entity_Name (N) then
                     if Present (Entity (N))
                       and then Present (Enclosing_Subprogram (Entity (N)))
                       and then Ekind (Entity (N)) /= E_Discriminant
                     then
                        Note_Uplevel_Ref
                          (E      => Entity (N),
                           N      => Ref,
                           Caller => Current_Subprogram,
                           Callee => Enclosing_Subprogram (Entity (N)));
                     end if;

                  --  Attribute or indexed component case

                  elsif Nkind_In (N, N_Attribute_Reference,
                                     N_Indexed_Component)
                  then
                     Note_Uplevel_Bound (Prefix (N), Ref);

                     --  The indices of the indexed components, or the
                     --  associated expressions of an attribute reference,
                     --  may also involve uplevel references.

                     declare
                        Expr : Node_Id;

                     begin
                        Expr := First (Expressions (N));
                        while Present (Expr) loop
                           Note_Uplevel_Bound (Expr, Ref);
                           Next (Expr);
                        end loop;
                     end;

                  --  Conversion case

                  elsif Nkind (N) = N_Type_Conversion then
                     Note_Uplevel_Bound (Expression (N), Ref);
                  end if;
               end Note_Uplevel_Bound;

            --  Start of processing for Check_Static_Type

            begin
               --  If already marked static, immediate return

               if Is_Static_Type (T) then
                  return;
               end if;

               --  If the type is at library level, always consider it static,
               --  since such uplevel references are irrelevant.

               if Is_Library_Level_Entity (T) then
                  Set_Is_Static_Type (T);
                  return;
               end if;

               --  Otherwise figure out what the story is with this type

               --  For a scalar type, check bounds

               if Is_Scalar_Type (T) then

                  --  If both bounds static, then this is a static type

                  declare
                     LB : constant Node_Id := Type_Low_Bound (T);
                     UB : constant Node_Id := Type_High_Bound (T);

                  begin
                     if not Is_Static_Expression (LB) then
                        Note_Uplevel_Bound (LB, N);
                        DT := True;
                     end if;

                     if not Is_Static_Expression (UB) then
                        Note_Uplevel_Bound (UB, N);
                        DT := True;
                     end if;
                  end;

               --  For record type, check all components and discriminant
               --  constraints if present.

               elsif Is_Record_Type (T) then
                  declare
                     C : Entity_Id;
                     D : Elmt_Id;

                  begin
                     C := First_Component_Or_Discriminant (T);
                     while Present (C) loop
                        Check_Static_Type (Etype (C), N, DT);
                        Next_Component_Or_Discriminant (C);
                     end loop;

                     if Has_Discriminants (T)
                       and then Present (Discriminant_Constraint (T))
                     then
                        D := First_Elmt (Discriminant_Constraint (T));
                        while Present (D) loop
                           if not Is_Static_Expression (Node (D)) then
                              Note_Uplevel_Bound (Node (D), N);
                              DT := True;
                           end if;

                           Next_Elmt (D);
                        end loop;
                     end if;
                  end;

               --  For array type, check index types and component type

               elsif Is_Array_Type (T) then
                  declare
                     IX : Node_Id;
                  begin
                     Check_Static_Type (Component_Type (T), N, DT);

                     IX := First_Index (T);
                     while Present (IX) loop
                        Check_Static_Type (Etype (IX), N, DT);
                        Next_Index (IX);
                     end loop;
                  end;

               --  For private type, examine whether full view is static

               elsif Is_Private_Type (T) and then Present (Full_View (T)) then
                  Check_Static_Type (Full_View (T), N, DT);

                  if Is_Static_Type (Full_View (T)) then
                     Set_Is_Static_Type (T);
                  end if;

               --  For now, ignore other types

               else
                  return;
               end if;

               if not DT then
                  Set_Is_Static_Type (T);
               end if;
            end Check_Static_Type;

            ----------------------
            -- Note_Uplevel_Ref --
            ----------------------

            procedure Note_Uplevel_Ref
              (E      : Entity_Id;
               N      : Node_Id;
               Caller : Entity_Id;
               Callee : Entity_Id)
            is
               Full_E : Entity_Id := E;
            begin
               --  Nothing to do for static type

               if Is_Static_Type (E) then
                  return;
               end if;

               --  Nothing to do if Caller and Callee are the same

               if Caller = Callee then
                  return;

               --  Callee may be a function that returns an array, and that has
               --  been rewritten as a procedure. If caller is that procedure,
               --  nothing to do either.

               elsif Ekind (Callee) = E_Function
                 and then Rewritten_For_C (Callee)
                 and then Corresponding_Procedure (Callee) = Caller
               then
                  return;
               end if;

               --  We have a new uplevel referenced entity

               if Ekind (E) = E_Constant and then Present (Full_View (E)) then
                  Full_E := Full_View (E);
               end if;

               --  All we do at this stage is to add the uplevel reference to
               --  the table. It's too early to do anything else, since this
               --  uplevel reference may come from an unreachable subprogram
               --  in which case the entry will be deleted.

               Urefs.Append ((N, Full_E, Caller, Callee));
            end Note_Uplevel_Ref;

            -------------------------
            -- Register_Subprogram --
            -------------------------

            procedure Register_Subprogram (E : Entity_Id; Bod : Node_Id) is
               L : constant Nat := Get_Level (Subp, E);

            begin
               Subps.Append
                 ((Ent           => E,
                   Bod           => Bod,
                   Lev           => L,
                   Reachable     => False,
                   Uplevel_Ref   => L,
                   Declares_AREC => False,
                   Uents         => No_Elist,
                   Last          => 0,
                   ARECnF        => Empty,
                   ARECn         => Empty,
                   ARECnT        => Empty,
                   ARECnPT       => Empty,
                   ARECnP        => Empty,
                   ARECnU        => Empty));

               Set_Subps_Index (E, UI_From_Int (Subps.Last));
            end Register_Subprogram;

         --  Start of processing for Visit_Node

         begin
            case Nkind (N) is

               --  Record a subprogram call

               when N_Function_Call
                  | N_Procedure_Call_Statement
               =>
                  --  We are only interested in direct calls, not indirect
                  --  calls (where Name (N) is an explicit dereference) at
                  --  least for now!

                  if Nkind (Name (N)) in N_Has_Entity then
                     Ent := Entity (Name (N));

                     --  We are only interested in calls to subprograms nested
                     --  within Subp. Calls to Subp itself or to subprograms
                     --  outside the nested structure do not affect us.

                     if Scope_Within (Ent, Subp)
                        and then Is_Subprogram (Ent)
                        and then not Is_Imported (Ent)
                     then
                        Append_Unique_Call ((N, Current_Subprogram, Ent));
                     end if;
                  end if;

                  --  For all calls where the formal is an unconstrained array
                  --  and the actual is constrained we need to check the bounds
                  --  for uplevel references.

                  declare
                     Actual : Entity_Id;
                     DT     : Boolean := False;
                     Formal : Node_Id;
                     Subp   : Entity_Id;

                  begin
                     if Nkind (Name (N)) = N_Explicit_Dereference then
                        Subp := Etype (Name (N));
                     else
                        Subp := Entity (Name (N));
                     end if;

                     Actual := First_Actual (N);
                     Formal := First_Formal_With_Extras (Subp);
                     while Present (Actual) loop
                        if Is_Array_Type (Etype (Formal))
                          and then not Is_Constrained (Etype (Formal))
                          and then Is_Constrained (Etype (Actual))
                        then
                           Check_Static_Type (Etype (Actual), Empty, DT);
                        end if;

                        Next_Actual (Actual);
                        Next_Formal_With_Extras (Formal);
                     end loop;
                  end;

               --  An At_End_Proc in a statement sequence indicates that there
               --  is a call from the enclosing construct or block to that
               --  subprogram. As above, the called entity must be local and
               --  not imported.

               when N_Handled_Sequence_Of_Statements =>
                  if Present (At_End_Proc (N))
                    and then Scope_Within (Entity (At_End_Proc (N)), Subp)
                    and then not Is_Imported (Entity (At_End_Proc (N)))
                  then
                     Append_Unique_Call
                       ((N, Current_Subprogram, Entity (At_End_Proc (N))));
                  end if;

               --  Similarly, the following constructs include a semantic
               --  attribute Procedure_To_Call that must be handled like
               --  other calls.

               when N_Allocator
                  | N_Extended_Return_Statement
                  | N_Free_Statement
                  | N_Simple_Return_Statement
               =>
                  declare
                     Proc : constant Entity_Id := Procedure_To_Call (N);
                  begin
                     if Present (Proc)
                       and then Scope_Within (Proc, Subp)
                       and then not Is_Imported (Proc)
                     then
                        Append_Unique_Call ((N, Current_Subprogram, Proc));
                     end if;
                  end;

                  --  For an allocator with a qualified expression, check type
                  --  of expression being qualified. The explicit type name is
                  --  handled as an entity reference.

                  if Nkind (N) = N_Allocator
                    and then Nkind (Expression (N)) = N_Qualified_Expression
                  then
                     declare
                        DT : Boolean := False;
                     begin
                        Check_Static_Type
                          (Etype (Expression (Expression (N))), Empty,  DT);
                     end;
                  end if;

               --  A 'Access reference is a (potential) call. Other attributes
               --  require special handling.

               when N_Attribute_Reference =>
                  declare
                     Attr : constant Attribute_Id :=
                              Get_Attribute_Id (Attribute_Name (N));
                  begin
                     case Attr is
                        when Attribute_Access
                           | Attribute_Unchecked_Access
                           | Attribute_Unrestricted_Access
                        =>
                           if Nkind (Prefix (N)) in N_Has_Entity then
                              Ent := Entity (Prefix (N));

                              --  We only need to examine calls to subprograms
                              --  nested within current Subp.

                              if Scope_Within (Ent, Subp) then
                                 if Is_Imported (Ent) then
                                    null;

                                 elsif Is_Subprogram (Ent) then
                                    Append_Unique_Call
                                      ((N, Current_Subprogram, Ent));
                                 end if;
                              end if;
                           end if;

                        --  References to bounds can be uplevel references if
                        --  the type isn't static.

                        when Attribute_First
                           | Attribute_Last
                           | Attribute_Length
                        =>
                           --  Special-case attributes of objects whose bounds
                           --  may be uplevel references. More complex prefixes
                           --  handled during full traversal. Note that if the
                           --  nominal subtype of the prefix is unconstrained,
                           --  the bound must be obtained from the object, not
                           --  from the (possibly) uplevel reference.

                           if Is_Constrained (Etype (Prefix (N))) then
                              declare
                                 DT : Boolean := False;
                              begin
                                 Check_Static_Type
                                   (Etype (Prefix (N)), Empty, DT);
                              end;

                              return OK;
                           end if;

                        when others =>
                           null;
                     end case;
                  end;

               --  Component associations in aggregates are either static or
               --  else the aggregate will be expanded into assignments, in
               --  which case the expression is analyzed later and provides
               --  no relevant code generation.

               when N_Component_Association =>
                  if No (Etype (Expression (N))) then
                     return Skip;
                  end if;

               --  Generic associations are not analyzed: the actuals are
               --  transferred to renaming and subtype declarations that
               --  are the ones that must be examined.

               when N_Generic_Association =>
                  return Skip;

               --  Indexed references can be uplevel if the type isn't static
               --  and if the lower bound (or an inner bound for a multi-
               --  dimensional array) is uplevel.

               when N_Indexed_Component | N_Slice =>
                  if Is_Constrained (Etype (Prefix (N))) then
                     declare
                        DT : Boolean := False;
                     begin
                        Check_Static_Type (Etype (Prefix (N)), Empty, DT);
                     end;
                  end if;

                  --  A selected component can have an implicit up-level
                  --  reference due to the bounds of previous fields in the
                  --  record. We simplify the processing here by examining
                  --  all components of the record.

                  --  Selected components appear as unit names and end labels
                  --  for child units. Prefixes of these nodes denote parent
                  --  units and carry no type information so they are skipped.

               when N_Selected_Component =>
                  if Present (Etype (Prefix (N))) then
                     declare
                        DT : Boolean := False;
                     begin
                        Check_Static_Type (Etype (Prefix (N)), Empty, DT);
                     end;
                  end if;

               --  Record a subprogram. We record a subprogram body that acts
               --  as a spec. Otherwise we record a subprogram declaration,
               --  providing that it has a corresponding body we can get hold
               --  of. The case of no corresponding body being available is
               --  ignored for now.

               when N_Subprogram_Body =>
                  Ent := Unique_Defining_Entity (N);

                  --  Ignore generic subprogram

                  if Is_Generic_Subprogram (Ent) then
                     return Skip;
                  end if;

                  --  Make new entry in subprogram table if not already made

                  Register_Subprogram (Ent, N);

                  --  We make a recursive call to scan the subprogram body, so
                  --  that we can save and restore Current_Subprogram.

                  declare
                     Save_CS : constant Entity_Id := Current_Subprogram;
                     Decl    : Node_Id;

                  begin
                     Current_Subprogram := Ent;

                     --  Scan declarations

                     Decl := First (Declarations (N));
                     while Present (Decl) loop
                        Visit (Decl);
                        Next (Decl);
                     end loop;

                     --  Scan statements

                     Visit (Handled_Statement_Sequence (N));

                     --  Restore current subprogram setting

                     Current_Subprogram := Save_CS;
                  end;

                  --  Now at this level, return skipping the subprogram body
                  --  descendants, since we already took care of them!

                  return Skip;

               --  If we have a body stub, visit the associated subunit, which
               --  is a semantic descendant of the stub.

               when N_Body_Stub =>
                  Visit (Library_Unit (N));

               --  A declaration of a wrapper package indicates a subprogram
               --  instance for which there is no explicit body. Enter the
               --  subprogram instance in the table.

               when N_Package_Declaration =>
                  if Is_Wrapper_Package (Defining_Entity (N)) then
                     Register_Subprogram
                       (Related_Instance (Defining_Entity (N)), Empty);
                  end if;

               --  Skip generic declarations

               when N_Generic_Declaration =>
                  return Skip;

               --  Skip generic package body

               when N_Package_Body =>
                  if Present (Corresponding_Spec (N))
                    and then Ekind (Corresponding_Spec (N)) = E_Generic_Package
                  then
                     return Skip;
                  end if;

               --  Otherwise record an uplevel reference in a local
               --  identifier.

               when others =>
                  if Nkind (N) in N_Has_Entity
                    and then Present (Entity (N))
                  then
                     Ent := Entity (N);

                     --  Only interested in entities declared within our nest

                     if not Is_Library_Level_Entity (Ent)
                       and then Scope_Within_Or_Same (Scope (Ent), Subp)

                        --  Skip entities defined in inlined subprograms

                       and then
                         Chars (Enclosing_Subprogram (Ent)) /= Name_uParent

                        --  Constants and variables are potentially uplevel
                        --  references to global declarations.

                       and then
                         (Ekind_In (Ent, E_Constant, E_Variable)

                        --  Formals are interesting, but not if being used as
                        --  mere names of parameters for name notation calls.

                        or else
                          (Is_Formal (Ent)
                            and then not
                             (Nkind (Parent (N)) = N_Parameter_Association
                               and then Selector_Name (Parent (N)) = N))

                        --  Types other than known Is_Static types are
                        --  potentially interesting.

                        or else (Is_Type (Ent)
                                  and then not Is_Static_Type (Ent)))
                     then
                        --  Here we have a potentially interesting uplevel
                        --  reference to examine.

                        if Is_Type (Ent) then
                           declare
                              DT : Boolean := False;

                           begin
                              Check_Static_Type (Ent, N, DT);

                              if Is_Static_Type (Ent) then
                                 return OK;
                              end if;
                           end;
                        end if;

                        Caller := Current_Subprogram;
                        Callee := Enclosing_Subprogram (Ent);

                        if Callee /= Caller
                          and then (not Is_Static_Type (Ent)
                                     or else Needs_Fat_Pointer (Ent))
                        then
                           Note_Uplevel_Ref (Ent, N, Caller, Callee);

                        --  Check the type of a formal parameter of the current
                        --  subprogram, whose formal type may be an uplevel
                        --  reference.

                        elsif Is_Formal (Ent)
                          and then Scope (Ent) = Current_Subprogram
                        then
                           declare
                              DT : Boolean := False;

                           begin
                              Check_Static_Type (Etype (Ent), Empty, DT);
                           end;
                        end if;
                     end if;
                  end if;
            end case;

            --  Fall through to continue scanning children of this node

            return OK;
         end Visit_Node;

      --  Start of processing for Build_Tables

      begin
         --  Traverse the body to get subprograms, calls and uplevel references

         Visit (Subp_Body);
      end Build_Tables;

      --  Now do the first transitive closure which determines which
      --  subprograms in the nest are actually reachable.

      Reachable_Closure : declare
         Modified : Boolean;

      begin
         Subps.Table (Subps_First).Reachable := True;

         --  We use a simple minded algorithm as follows (obviously this can
         --  be done more efficiently, using one of the standard algorithms
         --  for efficient transitive closure computation, but this is simple
         --  and most likely fast enough that its speed does not matter).

         --  Repeatedly scan the list of calls. Any time we find a call from
         --  A to B, where A is reachable, but B is not, then B is reachable,
         --  and note that we have made a change by setting Modified True. We
         --  repeat this until we make a pass with no modifications.

         Outer : loop
            Modified := False;
            Inner : for J in Calls.First .. Calls.Last loop
               declare
                  CTJ : Call_Entry renames Calls.Table (J);

                  SINF : constant SI_Type := Subp_Index (CTJ.Caller);
                  SINT : constant SI_Type := Subp_Index (CTJ.Callee);

                  SUBF : Subp_Entry renames Subps.Table (SINF);
                  SUBT : Subp_Entry renames Subps.Table (SINT);

               begin
                  if SUBF.Reachable and then not SUBT.Reachable then
                     SUBT.Reachable := True;
                     Modified := True;
                  end if;
               end;
            end loop Inner;

            exit Outer when not Modified;
         end loop Outer;
      end Reachable_Closure;

      --  Remove calls from unreachable subprograms

      declare
         New_Index : Nat;

      begin
         New_Index := 0;
         for J in Calls.First .. Calls.Last loop
            declare
               CTJ : Call_Entry renames Calls.Table (J);

               SINF : constant SI_Type := Subp_Index (CTJ.Caller);
               SINT : constant SI_Type := Subp_Index (CTJ.Callee);

               SUBF : Subp_Entry renames Subps.Table (SINF);
               SUBT : Subp_Entry renames Subps.Table (SINT);

            begin
               if SUBF.Reachable then
                  pragma Assert (SUBT.Reachable);
                  New_Index := New_Index + 1;
                  Calls.Table (New_Index) := Calls.Table (J);
               end if;
            end;
         end loop;

         Calls.Set_Last (New_Index);
      end;

      --  Remove uplevel references from unreachable subprograms

      declare
         New_Index : Nat;

      begin
         New_Index := 0;
         for J in Urefs.First .. Urefs.Last loop
            declare
               URJ : Uref_Entry renames Urefs.Table (J);

               SINF : constant SI_Type := Subp_Index (URJ.Caller);
               SINT : constant SI_Type := Subp_Index (URJ.Callee);

               SUBF : Subp_Entry renames Subps.Table (SINF);
               SUBT : Subp_Entry renames Subps.Table (SINT);

               S : Entity_Id;

            begin
               --  Keep reachable reference

               if SUBF.Reachable then
                  New_Index := New_Index + 1;
                  Urefs.Table (New_Index) := Urefs.Table (J);

                  --  And since we know we are keeping this one, this is a good
                  --  place to fill in information for a good reference.

                  --  Mark all enclosing subprograms need to declare AREC

                  S := URJ.Caller;
                  loop
                     S := Enclosing_Subprogram (S);

                     --  if we are at the top level, as can happen with
                     --  references to formals in aspects of nested subprogram
                     --  declarations, there are no further subprograms to
                     --  mark as requiring activation records.

                     exit when No (S);
                     Subps.Table (Subp_Index (S)).Declares_AREC := True;
                     exit when S = URJ.Callee;
                  end loop;

                  --  Add to list of uplevel referenced entities for Callee.
                  --  We do not add types to this list, only actual references
                  --  to objects that will be referenced uplevel, and we use
                  --  the flag Is_Uplevel_Referenced_Entity to avoid making
                  --  duplicate entries in the list.
                  --  Discriminants are also excluded, only the enclosing
                  --  object can appear in the list.

                  if not Is_Uplevel_Referenced_Entity (URJ.Ent)
                    and then Ekind (URJ.Ent) /= E_Discriminant
                  then
                     Set_Is_Uplevel_Referenced_Entity (URJ.Ent);

                     if not Is_Type (URJ.Ent) then
                        Append_New_Elmt (URJ.Ent, SUBT.Uents);
                     end if;
                  end if;

                  --  And set uplevel indication for caller

                  if SUBT.Lev < SUBF.Uplevel_Ref then
                     SUBF.Uplevel_Ref := SUBT.Lev;
                  end if;
               end if;
            end;
         end loop;

         Urefs.Set_Last (New_Index);
      end;

      --  Remove unreachable subprograms from Subps table. Note that we do
      --  this after eliminating entries from the other two tables, since
      --  those elimination steps depend on referencing the Subps table.

      declare
         New_SI : SI_Type;

      begin
         New_SI := Subps_First - 1;
         for J in Subps_First .. Subps.Last loop
            declare
               STJ  : Subp_Entry renames Subps.Table (J);
               Spec : Node_Id;
               Decl : Node_Id;

            begin
               --  Subprograms declared in tasks and protected types are
               --  reachable and cannot be eliminated.

               if In_Synchronized_Unit (STJ.Ent) then
                  STJ.Reachable := True;
               end if;

               --  Subprogram is reachable, copy and reset index

               if STJ.Reachable then
                  New_SI := New_SI + 1;
                  Subps.Table (New_SI) := STJ;
                  Set_Subps_Index (STJ.Ent, UI_From_Int (New_SI));

               --  Subprogram is not reachable

               else
                  --  Clear index, since no longer active

                  Set_Subps_Index (Subps.Table (J).Ent, Uint_0);

                  --  Output debug information if -gnatd.3 set

                  if Debug_Flag_Dot_3 then
                     Write_Str ("Eliminate ");
                     Write_Name (Chars (Subps.Table (J).Ent));
                     Write_Str (" at ");
                     Write_Location (Sloc (Subps.Table (J).Ent));
                     Write_Str (" (not referenced)");
                     Write_Eol;
                  end if;

                  --  Rewrite declaration and body to null statements

                  --  A subprogram instantiation does not have an explicit
                  --  body. If unused, we could remove the corresponding
                  --  wrapper package and its body (TBD).

                  if Present (STJ.Bod) then
                     Spec := Corresponding_Spec (STJ.Bod);

                     if Present (Spec) then
                        Decl := Parent (Declaration_Node (Spec));
                        Rewrite (Decl, Make_Null_Statement (Sloc (Decl)));
                     end if;

                     Rewrite (STJ.Bod, Make_Null_Statement (Sloc (STJ.Bod)));
                  end if;
               end if;
            end;
         end loop;

         Subps.Set_Last (New_SI);
      end;

      --  Now it is time for the second transitive closure, which follows calls
      --  and makes sure that A calls B, and B has uplevel references, then A
      --  is also marked as having uplevel references.

      Closure_Uplevel : declare
         Modified : Boolean;

      begin
         --  We use a simple minded algorithm as follows (obviously this can
         --  be done more efficiently, using one of the standard algorithms
         --  for efficient transitive closure computation, but this is simple
         --  and most likely fast enough that its speed does not matter).

         --  Repeatedly scan the list of calls. Any time we find a call from
         --  A to B, where B has uplevel references, make sure that A is marked
         --  as having at least the same level of uplevel referencing.

         Outer2 : loop
            Modified := False;
            Inner2 : for J in Calls.First .. Calls.Last loop
               declare
                  CTJ  : Call_Entry renames Calls.Table (J);
                  SINF : constant SI_Type := Subp_Index (CTJ.Caller);
                  SINT : constant SI_Type := Subp_Index (CTJ.Callee);
                  SUBF : Subp_Entry renames Subps.Table (SINF);
                  SUBT : Subp_Entry renames Subps.Table (SINT);
               begin
                  if SUBT.Lev > SUBT.Uplevel_Ref
                    and then SUBF.Uplevel_Ref > SUBT.Uplevel_Ref
                  then
                     SUBF.Uplevel_Ref := SUBT.Uplevel_Ref;
                     Modified := True;
                  end if;
               end;
            end loop Inner2;

            exit Outer2 when not Modified;
         end loop Outer2;
      end Closure_Uplevel;

      --  We have one more step before the tables are complete. An uplevel
      --  call from subprogram A to subprogram B where subprogram B has uplevel
      --  references is in effect an uplevel reference, and must arrange for
      --  the proper activation link to be passed.

      for J in Calls.First .. Calls.Last loop
         declare
            CTJ : Call_Entry renames Calls.Table (J);

            SINF : constant SI_Type := Subp_Index (CTJ.Caller);
            SINT : constant SI_Type := Subp_Index (CTJ.Callee);

            SUBF : Subp_Entry renames Subps.Table (SINF);
            SUBT : Subp_Entry renames Subps.Table (SINT);

            A : Entity_Id;

         begin
            --  If callee has uplevel references

            if SUBT.Uplevel_Ref < SUBT.Lev

              --  And this is an uplevel call

              and then SUBT.Lev < SUBF.Lev
            then
               --  We need to arrange for finding the uplink

               A := CTJ.Caller;
               loop
                  A := Enclosing_Subprogram (A);
                  Subps.Table (Subp_Index (A)).Declares_AREC := True;
                  exit when A = CTJ.Callee;

                  --  In any case exit when we get to the outer level. This
                  --  happens in some odd cases with generics (in particular
                  --  sem_ch3.adb does not compile without this kludge ???).

                  exit when A = Subp;
               end loop;
            end if;
         end;
      end loop;

      --  The tables are now complete, so we can record the last index in the
      --  Subps table for later reference in Cprint.

      Subps.Table (Subps_First).Last := Subps.Last;

      --  Next step, create the entities for code we will insert. We do this
      --  at the start so that all the entities are defined, regardless of the
      --  order in which we do the code insertions.

      Create_Entities : for J in Subps_First .. Subps.Last loop
         declare
            STJ : Subp_Entry renames Subps.Table (J);
            Loc : constant Source_Ptr := Sloc (STJ.Bod);

         begin
            --  First we create the ARECnF entity for the additional formal for
            --  all subprograms which need an activation record passed.

            if STJ.Uplevel_Ref < STJ.Lev then
               STJ.ARECnF :=
                 Make_Defining_Identifier (Loc, Chars => AREC_Name (J, "F"));
            end if;

            --  Define the AREC entities for the activation record if needed

            if STJ.Declares_AREC then
               STJ.ARECn   :=
                 Make_Defining_Identifier (Loc, AREC_Name (J, ""));
               STJ.ARECnT  :=
                 Make_Defining_Identifier (Loc, AREC_Name (J, "T"));
               STJ.ARECnPT :=
                 Make_Defining_Identifier (Loc, AREC_Name (J, "PT"));
               STJ.ARECnP  :=
                 Make_Defining_Identifier (Loc, AREC_Name (J, "P"));

               --  Define uplink component entity if inner nesting case

               if Present (STJ.ARECnF) then
                  STJ.ARECnU :=
                    Make_Defining_Identifier (Loc, AREC_Name (J, "U"));
               end if;
            end if;
         end;
      end loop Create_Entities;

      --  Loop through subprograms

      Subp_Loop : declare
         Addr : constant Entity_Id := RTE (RE_Address);

      begin
         for J in Subps_First .. Subps.Last loop
            declare
               STJ : Subp_Entry renames Subps.Table (J);

            begin
               --  First add the extra formal if needed. This applies to all
               --  nested subprograms that require an activation record to be
               --  passed, as indicated by ARECnF being defined.

               if Present (STJ.ARECnF) then

                  --  Here we need the extra formal. We do the expansion and
                  --  analysis of this manually, since it is fairly simple,
                  --  and it is not obvious how we can get what we want if we
                  --  try to use the normal Analyze circuit.

                  Add_Extra_Formal : declare
                     Encl : constant SI_Type := Enclosing_Subp (J);
                     STJE : Subp_Entry renames Subps.Table (Encl);
                     --  Index and Subp_Entry for enclosing routine

                     Form : constant Entity_Id := STJ.ARECnF;
                     --  The formal to be added. Note that n here is one less
                     --  than the level of the subprogram itself (STJ.Ent).

                     procedure Add_Form_To_Spec (F : Entity_Id; S : Node_Id);
                     --  S is an N_Function/Procedure_Specification node, and F
                     --  is the new entity to add to this subprogramn spec as
                     --  the last Extra_Formal.

                     ----------------------
                     -- Add_Form_To_Spec --
                     ----------------------

                     procedure Add_Form_To_Spec (F : Entity_Id; S : Node_Id) is
                        Sub : constant Entity_Id := Defining_Entity (S);
                        Ent : Entity_Id;

                     begin
                        --  Case of at least one Extra_Formal is present, set
                        --  ARECnF as the new last entry in the list.

                        if Present (Extra_Formals (Sub)) then
                           Ent := Extra_Formals (Sub);
                           while Present (Extra_Formal (Ent)) loop
                              Ent := Extra_Formal (Ent);
                           end loop;

                           Set_Extra_Formal (Ent, F);

                        --  No Extra formals present

                        else
                           Set_Extra_Formals (Sub, F);
                           Ent := Last_Formal (Sub);

                           if Present (Ent) then
                              Set_Extra_Formal (Ent, F);
                           end if;
                        end if;
                     end Add_Form_To_Spec;

                  --  Start of processing for Add_Extra_Formal

                  begin
                     --  Decorate the new formal entity

                     Set_Scope                (Form, STJ.Ent);
                     Set_Ekind                (Form, E_In_Parameter);
                     Set_Etype                (Form, STJE.ARECnPT);
                     Set_Mechanism            (Form, By_Copy);
                     Set_Never_Set_In_Source  (Form, True);
                     Set_Analyzed             (Form, True);
                     Set_Comes_From_Source    (Form, False);
                     Set_Is_Activation_Record (Form, True);

                     --  Case of only body present

                     if Acts_As_Spec (STJ.Bod) then
                        Add_Form_To_Spec (Form, Specification (STJ.Bod));

                     --  Case of separate spec

                     else
                        Add_Form_To_Spec (Form, Parent (STJ.Ent));
                     end if;
                  end Add_Extra_Formal;
               end if;

               --  Processing for subprograms that declare an activation record

               if Present (STJ.ARECn) then

                  --  Local declarations for one such subprogram

                  declare
                     Loc : constant Source_Ptr := Sloc (STJ.Bod);

                     Decls : constant List_Id := New_List;
                     --  List of new declarations we create

                     Clist : List_Id;
                     Comp  : Entity_Id;

                     Decl_Assign : Node_Id;
                     --  Assigment to set uplink, Empty if none

                     Decl_ARECnT  : Node_Id;
                     Decl_ARECnPT : Node_Id;
                     Decl_ARECn   : Node_Id;
                     Decl_ARECnP  : Node_Id;
                     --  Declaration nodes for the AREC entities we build

                  begin
                     --  Build list of component declarations for ARECnT

                     Clist := Empty_List;

                     --  If we are in a subprogram that has a static link that
                     --  is passed in (as indicated by ARECnF being defined),
                     --  then include ARECnU : ARECmPT where ARECmPT comes from
                     --  the level one higher than the current level, and the
                     --  entity ARECnPT comes from the enclosing subprogram.

                     if Present (STJ.ARECnF) then
                        declare
                           STJE : Subp_Entry
                                    renames Subps.Table (Enclosing_Subp (J));
                        begin
                           Append_To (Clist,
                             Make_Component_Declaration (Loc,
                               Defining_Identifier  => STJ.ARECnU,
                               Component_Definition =>
                                 Make_Component_Definition (Loc,
                                   Subtype_Indication =>
                                     New_Occurrence_Of (STJE.ARECnPT, Loc))));
                        end;
                     end if;

                     --  Add components for uplevel referenced entities

                     if Present (STJ.Uents) then
                        declare
                           Elmt     : Elmt_Id;
                           Ptr_Decl : Node_Id;
                           Uent     : Entity_Id;

                           Indx : Nat;
                           --  1's origin of index in list of elements. This is
                           --  used to uniquify names if needed in Upref_Name.

                        begin
                           Elmt := First_Elmt (STJ.Uents);
                           Indx := 0;
                           while Present (Elmt) loop
                              Uent := Node (Elmt);
                              Indx := Indx + 1;

                              Comp :=
                                Make_Defining_Identifier (Loc,
                                  Chars => Upref_Name (Uent, Indx, Clist));

                              Set_Activation_Record_Component
                                (Uent, Comp);

                              if Needs_Fat_Pointer (Uent) then

                                 --  Build corresponding access type

                                 Ptr_Decl :=
                                   Build_Access_Type_Decl
                                     (Etype (Uent), STJ.Ent);
                                 Append_To (Decls, Ptr_Decl);

                                 --  And use its type in the corresponding
                                 --  component.

                                 Append_To (Clist,
                                   Make_Component_Declaration (Loc,
                                     Defining_Identifier  => Comp,
                                     Component_Definition =>
                                       Make_Component_Definition (Loc,
                                         Subtype_Indication =>
                                           New_Occurrence_Of
                                             (Defining_Identifier (Ptr_Decl),
                                              Loc))));
                              else
                                 Append_To (Clist,
                                   Make_Component_Declaration (Loc,
                                     Defining_Identifier  => Comp,
                                     Component_Definition =>
                                       Make_Component_Definition (Loc,
                                         Subtype_Indication =>
                                           New_Occurrence_Of (Addr, Loc))));
                              end if;
                              Next_Elmt (Elmt);
                           end loop;
                        end;
                     end if;

                     --  Now we can insert the AREC declarations into the body
                     --    type ARECnT is record .. end record;
                     --    pragma Suppress_Initialization (ARECnT);

                     --  Note that we need to set the Suppress_Initialization
                     --  flag after Decl_ARECnT has been analyzed.

                     Decl_ARECnT :=
                       Make_Full_Type_Declaration (Loc,
                         Defining_Identifier => STJ.ARECnT,
                         Type_Definition     =>
                           Make_Record_Definition (Loc,
                             Component_List =>
                               Make_Component_List (Loc,
                                 Component_Items => Clist)));
                     Append_To (Decls, Decl_ARECnT);

                     --  type ARECnPT is access all ARECnT;

                     Decl_ARECnPT :=
                       Make_Full_Type_Declaration (Loc,
                         Defining_Identifier => STJ.ARECnPT,
                         Type_Definition     =>
                           Make_Access_To_Object_Definition (Loc,
                             All_Present        => True,
                             Subtype_Indication =>
                               New_Occurrence_Of (STJ.ARECnT, Loc)));
                     Append_To (Decls, Decl_ARECnPT);

                     --  ARECn : aliased ARECnT;

                     Decl_ARECn :=
                       Make_Object_Declaration (Loc,
                         Defining_Identifier => STJ.ARECn,
                           Aliased_Present   => True,
                           Object_Definition =>
                             New_Occurrence_Of (STJ.ARECnT, Loc));
                     Append_To (Decls, Decl_ARECn);

                     --  ARECnP : constant ARECnPT := ARECn'Access;

                     Decl_ARECnP :=
                       Make_Object_Declaration (Loc,
                         Defining_Identifier => STJ.ARECnP,
                         Constant_Present    => True,
                         Object_Definition   =>
                           New_Occurrence_Of (STJ.ARECnPT, Loc),
                         Expression          =>
                           Make_Attribute_Reference (Loc,
                             Prefix         =>
                               New_Occurrence_Of (STJ.ARECn, Loc),
                             Attribute_Name => Name_Access));
                     Append_To (Decls, Decl_ARECnP);

                     --  If we are in a subprogram that has a static link that
                     --  is passed in (as indicated by ARECnF being defined),
                     --  then generate ARECn.ARECmU := ARECmF where m is
                     --  one less than the current level to set the uplink.

                     if Present (STJ.ARECnF) then
                        Decl_Assign :=
                          Make_Assignment_Statement (Loc,
                            Name       =>
                              Make_Selected_Component (Loc,
                                Prefix        =>
                                  New_Occurrence_Of (STJ.ARECn, Loc),
                                Selector_Name =>
                                  New_Occurrence_Of (STJ.ARECnU, Loc)),
                            Expression =>
                              New_Occurrence_Of (STJ.ARECnF, Loc));
                        Append_To (Decls, Decl_Assign);

                     else
                        Decl_Assign := Empty;
                     end if;

                     Prepend_List_To (Declarations (STJ.Bod), Decls);

                     --  Analyze the newly inserted declarations. Note that we
                     --  do not need to establish the whole scope stack, since
                     --  we have already set all entity fields (so there will
                     --  be no searching of upper scopes to resolve names). But
                     --  we do set the scope of the current subprogram, so that
                     --  newly created entities go in the right entity chain.

                     --  We analyze with all checks suppressed (since we do
                     --  not expect any exceptions).

                     Push_Scope (STJ.Ent);
                     Analyze (Decl_ARECnT,  Suppress => All_Checks);

                     --  Note that we need to call Set_Suppress_Initialization
                     --  after Decl_ARECnT has been analyzed, but before
                     --  analyzing Decl_ARECnP so that the flag is properly
                     --  taking into account.

                     Set_Suppress_Initialization (STJ.ARECnT);

                     Analyze (Decl_ARECnPT, Suppress => All_Checks);
                     Analyze (Decl_ARECn,   Suppress => All_Checks);
                     Analyze (Decl_ARECnP,  Suppress => All_Checks);

                     if Present (Decl_Assign) then
                        Analyze (Decl_Assign, Suppress => All_Checks);
                     end if;

                     Pop_Scope;

                     --  Next step, for each uplevel referenced entity, add
                     --  assignment operations to set the component in the
                     --  activation record.

                     if Present (STJ.Uents) then
                        declare
                           Elmt : Elmt_Id;

                        begin
                           Elmt := First_Elmt (STJ.Uents);
                           while Present (Elmt) loop
                              declare
                                 Ent : constant Entity_Id  := Node (Elmt);
                                 Loc : constant Source_Ptr := Sloc (Ent);
                                 Dec : constant Node_Id    :=
                                         Declaration_Node (Ent);

                                 Asn  : Node_Id;
                                 Attr : Name_Id;
                                 Ins  : Node_Id;

                              begin
                                 --  For parameters, we insert the assignment
                                 --  right after the declaration of ARECnP.
                                 --  For all other entities, we insert
                                 --  the assignment immediately after the
                                 --  declaration of the entity.

                                 --  Note: we don't need to mark the entity
                                 --  as being aliased, because the address
                                 --  attribute will mark it as Address_Taken,
                                 --  and that is good enough.

                                 if Is_Formal (Ent) then
                                    Ins := Decl_ARECnP;
                                 else
                                    Ins := Dec;
                                 end if;

                                 --  Build and insert the assignment:
                                 --    ARECn.nam := nam'Address
                                 --  or else 'Access for unconstrained array

                                 if Needs_Fat_Pointer (Ent) then
                                    Attr := Name_Access;
                                 else
                                    Attr := Name_Address;
                                 end if;

                                 Asn :=
                                   Make_Assignment_Statement (Loc,
                                     Name       =>
                                       Make_Selected_Component (Loc,
                                         Prefix        =>
                                           New_Occurrence_Of (STJ.ARECn, Loc),
                                         Selector_Name =>
                                           New_Occurrence_Of
                                             (Activation_Record_Component
                                                (Ent),
                                              Loc)),

                                     Expression =>
                                       Make_Attribute_Reference (Loc,
                                         Prefix         =>
                                           New_Occurrence_Of (Ent, Loc),
                                         Attribute_Name => Attr));

                                 Insert_After (Ins, Asn);

                                 --  Analyze the assignment statement. We do
                                 --  not need to establish the relevant scope
                                 --  stack entries here, because we have
                                 --  already set the correct entity references,
                                 --  so no name resolution is required, and no
                                 --  new entities are created, so we don't even
                                 --  need to set the current scope.

                                 --  We analyze with all checks suppressed
                                 --  (since we do not expect any exceptions).

                                 Analyze (Asn, Suppress => All_Checks);
                              end;

                              Next_Elmt (Elmt);
                           end loop;
                        end;
                     end if;
                  end;
               end if;
            end;
         end loop;
      end Subp_Loop;

      --  Next step, process uplevel references. This has to be done in a
      --  separate pass, after completing the processing in Sub_Loop because we
      --  need all the AREC declarations generated, inserted, and analyzed so
      --  that the uplevel references can be successfully analyzed.

      Uplev_Refs : for J in Urefs.First .. Urefs.Last loop
         declare
            UPJ : Uref_Entry renames Urefs.Table (J);

         begin
            --  Ignore type references, these are implicit references that do
            --  not need rewriting (e.g. the appearence in a conversion).
            --  Also ignore if no reference was specified.

            if Is_Type (UPJ.Ent) or else No (UPJ.Ref) then
               goto Continue;
            end if;

            --  Also ignore uplevel references to bounds of types that come
            --  from the original type reference.

            if Is_Entity_Name (UPJ.Ref)
              and then Present (Entity (UPJ.Ref))
              and then Is_Type (Entity (UPJ.Ref))
            then
               goto Continue;
            end if;

            --  Rewrite one reference

            Rewrite_One_Ref : declare
               Loc : constant Source_Ptr := Sloc (UPJ.Ref);
               --  Source location for the reference

               Typ : constant Entity_Id := Etype (UPJ.Ent);
               --  The type of the referenced entity

               Atyp : constant Entity_Id := Get_Actual_Subtype (UPJ.Ref);
               --  The actual subtype of the reference

               RS_Caller : constant SI_Type := Subp_Index (UPJ.Caller);
               --  Subp_Index for caller containing reference

               STJR : Subp_Entry renames Subps.Table (RS_Caller);
               --  Subp_Entry for subprogram containing reference

               RS_Callee : constant SI_Type := Subp_Index (UPJ.Callee);
               --  Subp_Index for subprogram containing referenced entity

               STJE : Subp_Entry renames Subps.Table (RS_Callee);
               --  Subp_Entry for subprogram containing referenced entity

               Pfx  : Node_Id;
               Comp : Entity_Id;
               SI   : SI_Type;

            begin
               --  Ignore if no ARECnF entity for enclosing subprogram which
               --  probably happens as a result of not properly treating
               --  instance bodies. To be examined ???

               --  If this test is omitted, then the compilation of freeze.adb
               --  and inline.adb fail in unnesting mode.

               if No (STJR.ARECnF) then
                  goto Continue;
               end if;

               --  Push the current scope, so that the pointer type Tnn, and
               --  any subsidiary entities resulting from the analysis of the
               --  rewritten reference, go in the right entity chain.

               Push_Scope (STJR.Ent);

               --  Now we need to rewrite the reference. We have a reference
               --  from level STJR.Lev to level STJE.Lev. The general form of
               --  the rewritten reference for entity X is:

               --    Typ'Deref (ARECaF.ARECbU.ARECcU.ARECdU....ARECmU.X)

               --  where a,b,c,d .. m =
               --    STJR.Lev - 1,  STJR.Lev - 2, .. STJE.Lev

               pragma Assert (STJR.Lev > STJE.Lev);

               --  Compute the prefix of X. Here are examples to make things
               --  clear (with parens to show groupings, the prefix is
               --  everything except the .X at the end).

               --   level 2 to level 1

               --     AREC1F.X

               --   level 3 to level 1

               --     (AREC2F.AREC1U).X

               --   level 4 to level 1

               --     ((AREC3F.AREC2U).AREC1U).X

               --   level 6 to level 2

               --     (((AREC5F.AREC4U).AREC3U).AREC2U).X

               --  In the above, ARECnF and ARECnU are pointers, so there are
               --  explicit dereferences required for these occurrences.

               Pfx :=
                 Make_Explicit_Dereference (Loc,
                   Prefix => New_Occurrence_Of (STJR.ARECnF, Loc));
               SI := RS_Caller;
               for L in STJE.Lev .. STJR.Lev - 2 loop
                  SI := Enclosing_Subp (SI);
                  Pfx :=
                    Make_Explicit_Dereference (Loc,
                      Prefix =>
                        Make_Selected_Component (Loc,
                          Prefix        => Pfx,
                          Selector_Name =>
                            New_Occurrence_Of (Subps.Table (SI).ARECnU, Loc)));
               end loop;

               --  Get activation record component (must exist)

               Comp := Activation_Record_Component (UPJ.Ent);
               pragma Assert (Present (Comp));

               --  Do the replacement. If the component type is an access type,
               --  this is an uplevel reference for an entity that requires a
               --  fat pointer, so dereference the component.

               if Is_Access_Type (Etype (Comp)) then
                  Rewrite (UPJ.Ref,
                    Make_Explicit_Dereference (Loc,
                      Prefix =>
                        Make_Selected_Component (Loc,
                          Prefix        => Pfx,
                          Selector_Name =>
                            New_Occurrence_Of (Comp, Loc))));

               else
                  Rewrite (UPJ.Ref,
                    Make_Attribute_Reference (Loc,
                      Prefix         => New_Occurrence_Of (Atyp, Loc),
                      Attribute_Name => Name_Deref,
                      Expressions    => New_List (
                        Make_Selected_Component (Loc,
                          Prefix        => Pfx,
                          Selector_Name =>
                            New_Occurrence_Of (Comp, Loc)))));
               end if;

               --  Analyze and resolve the new expression. We do not need to
               --  establish the relevant scope stack entries here, because we
               --  have already set all the correct entity references, so no
               --  name resolution is needed. We have already set the current
               --  scope, so that any new entities created will be in the right
               --  scope.

               --  We analyze with all checks suppressed (since we do not
               --  expect any exceptions)

               Analyze_And_Resolve (UPJ.Ref, Typ, Suppress => All_Checks);
               Pop_Scope;
            end Rewrite_One_Ref;
         end;

      <<Continue>>
         null;
      end loop Uplev_Refs;

      --  Finally, loop through all calls adding extra actual for the
      --  activation record where it is required.

      Adjust_Calls : for J in Calls.First .. Calls.Last loop

         --  Process a single call, we are only interested in a call to a
         --  subprogram that actually needs a pointer to an activation record,
         --  as indicated by the ARECnF entity being set. This excludes the
         --  top level subprogram, and any subprogram not having uplevel refs.

         Adjust_One_Call : declare
            CTJ : Call_Entry renames Calls.Table (J);
            STF : Subp_Entry renames Subps.Table (Subp_Index (CTJ.Caller));
            STT : Subp_Entry renames Subps.Table (Subp_Index (CTJ.Callee));

            Loc : constant Source_Ptr := Sloc (CTJ.N);

            Extra  : Node_Id;
            ExtraP : Node_Id;
            SubX   : SI_Type;
            Act    : Node_Id;

         begin
            if Present (STT.ARECnF)
              and then Nkind (CTJ.N) in N_Subprogram_Call
            then
               --  CTJ.N is a call to a subprogram which may require a pointer
               --  to an activation record. The subprogram containing the call
               --  is CTJ.From and the subprogram being called is CTJ.To, so we
               --  have a call from level STF.Lev to level STT.Lev.

               --  There are three possibilities:

               --  For a call to the same level, we just pass the activation
               --  record passed to the calling subprogram.

               if STF.Lev = STT.Lev then
                  Extra := New_Occurrence_Of (STF.ARECnF, Loc);

               --  For a call that goes down a level, we pass a pointer to the
               --  activation record constructed within the caller (which may
               --  be the outer-level subprogram, but also may be a more deeply
               --  nested caller).

               elsif STT.Lev = STF.Lev + 1 then
                  Extra := New_Occurrence_Of (STF.ARECnP, Loc);

                  --  Otherwise we must have an upcall (STT.Lev < STF.LEV),
                  --  since it is not possible to do a downcall of more than
                  --  one level.

                  --  For a call from level STF.Lev to level STT.Lev, we
                  --  have to find the activation record needed by the
                  --  callee. This is as follows:

                  --    ARECaF.ARECbU.ARECcU....ARECmU

                  --  where a,b,c .. m =
                  --    STF.Lev - 1,  STF.Lev - 2, STF.Lev - 3 .. STT.Lev

               else
                  pragma Assert (STT.Lev < STF.Lev);

                  Extra := New_Occurrence_Of (STF.ARECnF, Loc);
                  SubX  := Subp_Index (CTJ.Caller);
                  for K in reverse STT.Lev .. STF.Lev - 1 loop
                     SubX  := Enclosing_Subp (SubX);
                     Extra :=
                       Make_Selected_Component (Loc,
                         Prefix        => Extra,
                         Selector_Name =>
                           New_Occurrence_Of
                             (Subps.Table (SubX).ARECnU, Loc));
                  end loop;
               end if;

               --  Extra is the additional parameter to be added. Build a
               --  parameter association that we can append to the actuals.

               ExtraP :=
                 Make_Parameter_Association (Loc,
                   Selector_Name             =>
                     New_Occurrence_Of (STT.ARECnF, Loc),
                   Explicit_Actual_Parameter => Extra);

               if No (Parameter_Associations (CTJ.N)) then
                  Set_Parameter_Associations (CTJ.N, Empty_List);
               end if;

               Append (ExtraP, Parameter_Associations (CTJ.N));

               --  We need to deal with the actual parameter chain as well. The
               --  newly added parameter is always the last actual.

               Act := First_Named_Actual (CTJ.N);

               if No (Act) then
                  Set_First_Named_Actual (CTJ.N, Extra);

                  --  If call has been relocated (as with an expression in
                  --  an aggregate), set First_Named pointer in original node
                  --  as well, because that's the parent of the parameter list.

                  Set_First_Named_Actual
                    (Parent (List_Containing (ExtraP)), Extra);

               --  Here we must follow the chain and append the new entry

               else
                  loop
                     declare
                        PAN : Node_Id;
                        NNA : Node_Id;

                     begin
                        PAN := Parent (Act);
                        pragma Assert (Nkind (PAN) = N_Parameter_Association);
                        NNA := Next_Named_Actual (PAN);

                        if No (NNA) then
                           Set_Next_Named_Actual (PAN, Extra);
                           exit;
                        end if;

                        Act := NNA;
                     end;
                  end loop;
               end if;

               --  Analyze and resolve the new actual. We do not need to
               --  establish the relevant scope stack entries here, because
               --  we have already set all the correct entity references, so
               --  no name resolution is needed.

               --  We analyze with all checks suppressed (since we do not
               --  expect any exceptions, and also we temporarily turn off
               --  Unested_Subprogram_Mode to avoid trying to mark uplevel
               --  references (not needed at this stage, and in fact causes
               --  a bit of recursive chaos).

               Opt.Unnest_Subprogram_Mode := False;
               Analyze_And_Resolve
                 (Extra, Etype (STT.ARECnF), Suppress => All_Checks);
               Opt.Unnest_Subprogram_Mode := True;
            end if;
         end Adjust_One_Call;
      end loop Adjust_Calls;

      return;
   end Unnest_Subprogram;

   ------------------------
   -- Unnest_Subprograms --
   ------------------------

   procedure Unnest_Subprograms (N : Node_Id) is
      function Search_Subprograms (N : Node_Id) return Traverse_Result;
      --  Tree visitor that search for outer level procedures with nested
      --  subprograms and invokes Unnest_Subprogram()

      ---------------
      -- Do_Search --
      ---------------

      procedure Do_Search is new Traverse_Proc (Search_Subprograms);
      --  Subtree visitor instantiation

      ------------------------
      -- Search_Subprograms --
      ------------------------

      function Search_Subprograms (N : Node_Id) return Traverse_Result is
      begin
         if Nkind_In (N, N_Subprogram_Body, N_Subprogram_Body_Stub) then
            declare
               Spec_Id : constant Entity_Id := Unique_Defining_Entity (N);

            begin
               --  We are only interested in subprograms (not generic
               --  subprograms), that have nested subprograms.

               if Is_Subprogram (Spec_Id)
                 and then Has_Nested_Subprogram (Spec_Id)
                 and then Is_Library_Level_Entity (Spec_Id)
               then
                  Unnest_Subprogram (Spec_Id, N);
               end if;
            end;
         end if;

         --  The proper body of a stub may contain nested subprograms, and
         --  therefore must be visited explicitly. Nested stubs are examined
         --  recursively in Visit_Node.

         if Nkind (N) in N_Body_Stub then
            Do_Search (Library_Unit (N));
         end if;

         return OK;
      end Search_Subprograms;

   --  Start of processing for Unnest_Subprograms

   begin
      if not Opt.Unnest_Subprogram_Mode then
         return;
      end if;

      --  A specification will contain bodies if it contains instantiations so
      --  examine package or subprogram declaration of the main unit, when it
      --  is present.

      if Nkind (Unit (N)) = N_Package_Body
        or else (Nkind (Unit (N)) = N_Subprogram_Body
                  and then not Acts_As_Spec (N))
      then
         Do_Search (Library_Unit (N));
      end if;

      Do_Search (N);
   end Unnest_Subprograms;

end Exp_Unst;
