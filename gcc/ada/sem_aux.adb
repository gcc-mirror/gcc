------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              S E M _ A U X                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2019, Free Software Foundation, Inc.         --
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
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Atree;  use Atree;
with Einfo;  use Einfo;
with Snames; use Snames;
with Stand;  use Stand;
with Uintp;  use Uintp;

package body Sem_Aux is

   ----------------------
   -- Ancestor_Subtype --
   ----------------------

   function Ancestor_Subtype (Typ : Entity_Id) return Entity_Id is
   begin
      --  If this is first subtype, or is a base type, then there is no
      --  ancestor subtype, so we return Empty to indicate this fact.

      if Is_First_Subtype (Typ) or else Is_Base_Type (Typ) then
         return Empty;
      end if;

      declare
         D : constant Node_Id := Declaration_Node (Typ);

      begin
         --  If we have a subtype declaration, get the ancestor subtype

         if Nkind (D) = N_Subtype_Declaration then
            if Nkind (Subtype_Indication (D)) = N_Subtype_Indication then
               return Entity (Subtype_Mark (Subtype_Indication (D)));
            else
               return Entity (Subtype_Indication (D));
            end if;

         --  If not, then no subtype indication is available

         else
            return Empty;
         end if;
      end;
   end Ancestor_Subtype;

   --------------------
   -- Available_View --
   --------------------

   function Available_View (Ent : Entity_Id) return Entity_Id is
   begin
      --  Obtain the non-limited view (if available)

      if Has_Non_Limited_View (Ent) then
         return Get_Full_View (Non_Limited_View (Ent));

      --  In all other cases, return entity unchanged

      else
         return Ent;
      end if;
   end Available_View;

   --------------------
   -- Constant_Value --
   --------------------

   function Constant_Value (Ent : Entity_Id) return Node_Id is
      D      : constant Node_Id := Declaration_Node (Ent);
      Full_D : Node_Id;

   begin
      --  If we have no declaration node, then return no constant value. Not
      --  clear how this can happen, but it does sometimes and this is the
      --  safest approach.

      if No (D) then
         return Empty;

      --  Normal case where a declaration node is present

      elsif Nkind (D) = N_Object_Renaming_Declaration then
         return Renamed_Object (Ent);

      --  If this is a component declaration whose entity is a constant, it is
      --  a prival within a protected function (and so has no constant value).

      elsif Nkind (D) = N_Component_Declaration then
         return Empty;

      --  If there is an expression, return it

      elsif Present (Expression (D)) then
         return Expression (D);

      --  For a constant, see if we have a full view

      elsif Ekind (Ent) = E_Constant
        and then Present (Full_View (Ent))
      then
         Full_D := Parent (Full_View (Ent));

         --  The full view may have been rewritten as an object renaming

         if Nkind (Full_D) = N_Object_Renaming_Declaration then
            return Name (Full_D);
         else
            return Expression (Full_D);
         end if;

      --  Otherwise we have no expression to return

      else
         return Empty;
      end if;
   end Constant_Value;

   ---------------------------------
   -- Corresponding_Unsigned_Type --
   ---------------------------------

   function Corresponding_Unsigned_Type (Typ : Entity_Id) return Entity_Id is
      pragma Assert (Is_Signed_Integer_Type (Typ));
      Siz : constant Uint := Esize (Base_Type (Typ));
   begin
      if Siz = Esize (Standard_Short_Short_Integer) then
         return Standard_Short_Short_Unsigned;
      elsif Siz = Esize (Standard_Short_Integer) then
         return Standard_Short_Unsigned;
      elsif Siz = Esize (Standard_Unsigned) then
         return Standard_Unsigned;
      elsif Siz = Esize (Standard_Long_Integer) then
         return Standard_Long_Unsigned;
      elsif Siz = Esize (Standard_Long_Long_Integer) then
         return Standard_Long_Long_Unsigned;
      else
         raise Program_Error;
      end if;
   end Corresponding_Unsigned_Type;

   -----------------------------
   -- Enclosing_Dynamic_Scope --
   -----------------------------

   function Enclosing_Dynamic_Scope (Ent : Entity_Id) return Entity_Id is
      S : Entity_Id;

   begin
      --  The following test is an error defense against some syntax errors
      --  that can leave scopes very messed up.

      if Ent = Standard_Standard then
         return Ent;
      end if;

      --  Normal case, search enclosing scopes

      --  Note: the test for Present (S) should not be required, it defends
      --  against an ill-formed tree.

      S := Scope (Ent);
      loop
         --  If we somehow got an empty value for Scope, the tree must be
         --  malformed. Rather than blow up we return Standard in this case.

         if No (S) then
            return Standard_Standard;

         --  Quit if we get to standard or a dynamic scope. We must also
         --  handle enclosing scopes that have a full view; required to
         --  locate enclosing scopes that are synchronized private types
         --  whose full view is a task type.

         elsif S = Standard_Standard
           or else Is_Dynamic_Scope (S)
           or else (Is_Private_Type (S)
                     and then Present (Full_View (S))
                     and then Is_Dynamic_Scope (Full_View (S)))
         then
            return S;

         --  Otherwise keep climbing

         else
            S := Scope (S);
         end if;
      end loop;
   end Enclosing_Dynamic_Scope;

   ------------------------
   -- First_Discriminant --
   ------------------------

   function First_Discriminant (Typ : Entity_Id) return Entity_Id is
      Ent : Entity_Id;

   begin
      pragma Assert
        (Has_Discriminants (Typ) or else Has_Unknown_Discriminants (Typ));

      Ent := First_Entity (Typ);

      --  The discriminants are not necessarily contiguous, because access
      --  discriminants will generate itypes. They are not the first entities
      --  either because the tag must be ahead of them.

      if Chars (Ent) = Name_uTag then
         Ent := Next_Entity (Ent);
      end if;

      --  Skip all hidden stored discriminants if any

      while Present (Ent) loop
         exit when Ekind (Ent) = E_Discriminant
           and then not Is_Completely_Hidden (Ent);

         Ent := Next_Entity (Ent);
      end loop;

      --  Call may be on a private type with unknown discriminants, in which
      --  case Ent is Empty, and as per the spec, we return Empty in this case.

      --  Historical note: The assertion in previous versions that Ent is a
      --  discriminant was overly cautious and prevented convenient application
      --  of this function in the gnatprove context.

      return Ent;
   end First_Discriminant;

   -------------------------------
   -- First_Stored_Discriminant --
   -------------------------------

   function First_Stored_Discriminant (Typ : Entity_Id) return Entity_Id is
      Ent : Entity_Id;

      function Has_Completely_Hidden_Discriminant
        (Typ : Entity_Id) return Boolean;
      --  Scans the Discriminants to see whether any are Completely_Hidden
      --  (the mechanism for describing non-specified stored discriminants)
      --  Note that the entity list for the type may contain anonymous access
      --  types created by expressions that constrain access discriminants.

      ----------------------------------------
      -- Has_Completely_Hidden_Discriminant --
      ----------------------------------------

      function Has_Completely_Hidden_Discriminant
        (Typ : Entity_Id) return Boolean
      is
         Ent : Entity_Id;

      begin
         pragma Assert (Ekind (Typ) = E_Discriminant);

         Ent := Typ;
         while Present (Ent) loop

            --  Skip anonymous types that may be created by expressions
            --  used as discriminant constraints on inherited discriminants.

            if Is_Itype (Ent) then
               null;

            elsif Ekind (Ent) = E_Discriminant
              and then Is_Completely_Hidden (Ent)
            then
               return True;
            end if;

            Ent := Next_Entity (Ent);
         end loop;

         return False;
      end Has_Completely_Hidden_Discriminant;

   --  Start of processing for First_Stored_Discriminant

   begin
      pragma Assert
        (Has_Discriminants (Typ)
          or else Has_Unknown_Discriminants (Typ));

      Ent := First_Entity (Typ);

      if Chars (Ent) = Name_uTag then
         Ent := Next_Entity (Ent);
      end if;

      if Has_Completely_Hidden_Discriminant (Ent) then
         while Present (Ent) loop
            exit when Ekind (Ent) = E_Discriminant
              and then Is_Completely_Hidden (Ent);
            Ent := Next_Entity (Ent);
         end loop;
      end if;

      pragma Assert (Ekind (Ent) = E_Discriminant);

      return Ent;
   end First_Stored_Discriminant;

   -------------------
   -- First_Subtype --
   -------------------

   function First_Subtype (Typ : Entity_Id) return Entity_Id is
      B   : constant Entity_Id := Base_Type (Typ);
      F   : constant Node_Id   := Freeze_Node (B);
      Ent : Entity_Id;

   begin
      --  If the base type has no freeze node, it is a type in Standard, and
      --  always acts as its own first subtype, except where it is one of the
      --  predefined integer types. If the type is formal, it is also a first
      --  subtype, and its base type has no freeze node. On the other hand, a
      --  subtype of a generic formal is not its own first subtype. Its base
      --  type, if anonymous, is attached to the formal type decl. from which
      --  the first subtype is obtained.

      if No (F) then
         if B = Base_Type (Standard_Integer) then
            return Standard_Integer;

         elsif B = Base_Type (Standard_Long_Integer) then
            return Standard_Long_Integer;

         elsif B = Base_Type (Standard_Short_Short_Integer) then
            return Standard_Short_Short_Integer;

         elsif B = Base_Type (Standard_Short_Integer) then
            return Standard_Short_Integer;

         elsif B = Base_Type (Standard_Long_Long_Integer) then
            return Standard_Long_Long_Integer;

         elsif Is_Generic_Type (Typ) then
            if Present (Parent (B)) then
               return Defining_Identifier (Parent (B));
            else
               return Defining_Identifier (Associated_Node_For_Itype (B));
            end if;

         else
            return B;
         end if;

      --  Otherwise we check the freeze node, if it has a First_Subtype_Link
      --  then we use that link, otherwise (happens with some Itypes), we use
      --  the base type itself.

      else
         Ent := First_Subtype_Link (F);

         if Present (Ent) then
            return Ent;
         else
            return B;
         end if;
      end if;
   end First_Subtype;

   -------------------------
   -- First_Tag_Component --
   -------------------------

   function First_Tag_Component (Typ : Entity_Id) return Entity_Id is
      Comp : Entity_Id;
      Ctyp : Entity_Id;

   begin
      Ctyp := Typ;
      pragma Assert (Is_Tagged_Type (Ctyp));

      if Is_Class_Wide_Type (Ctyp) then
         Ctyp := Root_Type (Ctyp);
      end if;

      if Is_Private_Type (Ctyp) then
         Ctyp := Underlying_Type (Ctyp);

         --  If the underlying type is missing then the source program has
         --  errors and there is nothing else to do (the full-type declaration
         --  associated with the private type declaration is missing).

         if No (Ctyp) then
            return Empty;
         end if;
      end if;

      Comp := First_Entity (Ctyp);
      while Present (Comp) loop
         if Is_Tag (Comp) then
            return Comp;
         end if;

         Comp := Next_Entity (Comp);
      end loop;

      --  No tag component found

      return Empty;
   end First_Tag_Component;

   ---------------------
   -- Get_Binary_Nkind --
   ---------------------

   function Get_Binary_Nkind (Op : Entity_Id) return Node_Kind is
   begin
      case Chars (Op) is
         when Name_Op_Add      => return N_Op_Add;
         when Name_Op_Concat   => return N_Op_Concat;
         when Name_Op_Expon    => return N_Op_Expon;
         when Name_Op_Subtract => return N_Op_Subtract;
         when Name_Op_Mod      => return N_Op_Mod;
         when Name_Op_Multiply => return N_Op_Multiply;
         when Name_Op_Divide   => return N_Op_Divide;
         when Name_Op_Rem      => return N_Op_Rem;
         when Name_Op_And      => return N_Op_And;
         when Name_Op_Eq       => return N_Op_Eq;
         when Name_Op_Ge       => return N_Op_Ge;
         when Name_Op_Gt       => return N_Op_Gt;
         when Name_Op_Le       => return N_Op_Le;
         when Name_Op_Lt       => return N_Op_Lt;
         when Name_Op_Ne       => return N_Op_Ne;
         when Name_Op_Or       => return N_Op_Or;
         when Name_Op_Xor      => return N_Op_Xor;
         when others           => raise Program_Error;
      end case;
   end Get_Binary_Nkind;

   -----------------------
   -- Get_Called_Entity --
   -----------------------

   function Get_Called_Entity (Call : Node_Id) return Entity_Id is
      Nam : constant Node_Id := Name (Call);
      Id  : Entity_Id;

   begin
      if Nkind (Nam) = N_Explicit_Dereference then
         Id := Etype (Nam);
         pragma Assert (Ekind (Id) = E_Subprogram_Type);

      elsif Nkind (Nam) = N_Selected_Component then
         Id := Entity (Selector_Name (Nam));

      elsif Nkind (Nam) = N_Indexed_Component then
         Id := Entity (Selector_Name (Prefix (Nam)));

      else
         Id := Entity (Nam);
      end if;

      return Id;
   end Get_Called_Entity;

   -------------------
   -- Get_Low_Bound --
   -------------------

   function Get_Low_Bound (E : Entity_Id) return Node_Id is
   begin
      if Ekind (E) = E_String_Literal_Subtype then
         return String_Literal_Low_Bound (E);
      else
         return Type_Low_Bound (E);
      end if;
   end Get_Low_Bound;

   ------------------
   -- Get_Rep_Item --
   ------------------

   function Get_Rep_Item
     (E             : Entity_Id;
      Nam           : Name_Id;
      Check_Parents : Boolean := True) return Node_Id
   is
      N : Node_Id;

   begin
      N := First_Rep_Item (E);
      while Present (N) loop

         --  Only one of Priority / Interrupt_Priority can be specified, so
         --  return whichever one is present to catch illegal duplication.

         if Nkind (N) = N_Pragma
           and then
             (Pragma_Name_Unmapped (N) = Nam
               or else (Nam = Name_Priority
                         and then Pragma_Name (N) =
                           Name_Interrupt_Priority)
               or else (Nam = Name_Interrupt_Priority
                         and then Pragma_Name (N) = Name_Priority))
         then
            if Check_Parents then
               return N;

            --  If Check_Parents is False, return N if the pragma doesn't
            --  appear in the Rep_Item chain of the parent.

            else
               declare
                  Par : constant Entity_Id := Nearest_Ancestor (E);
                  --  This node represents the parent type of type E (if any)

               begin
                  if No (Par) then
                     return N;

                  elsif not Present_In_Rep_Item (Par, N) then
                     return N;
                  end if;
               end;
            end if;

         elsif Nkind (N) = N_Attribute_Definition_Clause
           and then
             (Chars (N) = Nam
               or else (Nam = Name_Priority
                         and then Chars (N) = Name_Interrupt_Priority))
         then
            if Check_Parents or else Entity (N) = E then
               return N;
            end if;

         elsif Nkind (N) = N_Aspect_Specification
           and then
             (Chars (Identifier (N)) = Nam
               or else
                 (Nam = Name_Priority
                   and then Chars (Identifier (N)) = Name_Interrupt_Priority))
         then
            if Check_Parents then
               return N;

            elsif Entity (N) = E then
               return N;
            end if;

         --  A Ghost-related aspect, if disabled, may have been replaced by a
         --  null statement.

         elsif Nkind (N) = N_Null_Statement then
            N := Original_Node (N);
         end if;

         Next_Rep_Item (N);
      end loop;

      return Empty;
   end Get_Rep_Item;

   function Get_Rep_Item
     (E             : Entity_Id;
      Nam1          : Name_Id;
      Nam2          : Name_Id;
      Check_Parents : Boolean := True) return Node_Id
   is
      Nam1_Item : constant Node_Id := Get_Rep_Item (E, Nam1, Check_Parents);
      Nam2_Item : constant Node_Id := Get_Rep_Item (E, Nam2, Check_Parents);

      N : Node_Id;

   begin
      --  Check both Nam1_Item and Nam2_Item are present

      if No (Nam1_Item) then
         return Nam2_Item;
      elsif No (Nam2_Item) then
         return Nam1_Item;
      end if;

      --  Return the first node encountered in the list

      N := First_Rep_Item (E);
      while Present (N) loop
         if N = Nam1_Item or else N = Nam2_Item then
            return N;
         end if;

         Next_Rep_Item (N);
      end loop;

      return Empty;
   end Get_Rep_Item;

   --------------------
   -- Get_Rep_Pragma --
   --------------------

   function Get_Rep_Pragma
     (E             : Entity_Id;
      Nam           : Name_Id;
      Check_Parents : Boolean := True) return Node_Id
   is
      N : constant Node_Id := Get_Rep_Item (E, Nam, Check_Parents);

   begin
      if Present (N) and then Nkind (N) = N_Pragma then
         return N;
      end if;

      return Empty;
   end Get_Rep_Pragma;

   function Get_Rep_Pragma
     (E             : Entity_Id;
      Nam1          : Name_Id;
      Nam2          : Name_Id;
      Check_Parents : Boolean := True) return Node_Id
   is
      Nam1_Item : constant Node_Id := Get_Rep_Pragma (E, Nam1, Check_Parents);
      Nam2_Item : constant Node_Id := Get_Rep_Pragma (E, Nam2, Check_Parents);

      N : Node_Id;

   begin
      --  Check both Nam1_Item and Nam2_Item are present

      if No (Nam1_Item) then
         return Nam2_Item;
      elsif No (Nam2_Item) then
         return Nam1_Item;
      end if;

      --  Return the first node encountered in the list

      N := First_Rep_Item (E);
      while Present (N) loop
         if N = Nam1_Item or else N = Nam2_Item then
            return N;
         end if;

         Next_Rep_Item (N);
      end loop;

      return Empty;
   end Get_Rep_Pragma;

   ---------------------
   -- Get_Unary_Nkind --
   ---------------------

   function Get_Unary_Nkind (Op : Entity_Id) return Node_Kind is
   begin
      case Chars (Op) is
         when Name_Op_Abs      => return N_Op_Abs;
         when Name_Op_Subtract => return N_Op_Minus;
         when Name_Op_Not      => return N_Op_Not;
         when Name_Op_Add      => return N_Op_Plus;
         when others           => raise Program_Error;
      end case;
   end Get_Unary_Nkind;

   ---------------------------------
   -- Has_External_Tag_Rep_Clause --
   ---------------------------------

   function Has_External_Tag_Rep_Clause (T : Entity_Id) return Boolean is
   begin
      pragma Assert (Is_Tagged_Type (T));
      return Has_Rep_Item (T, Name_External_Tag, Check_Parents => False);
   end Has_External_Tag_Rep_Clause;

   ------------------
   -- Has_Rep_Item --
   ------------------

   function Has_Rep_Item
     (E             : Entity_Id;
      Nam           : Name_Id;
      Check_Parents : Boolean := True) return Boolean
   is
   begin
      return Present (Get_Rep_Item (E, Nam, Check_Parents));
   end Has_Rep_Item;

   function Has_Rep_Item
     (E             : Entity_Id;
      Nam1          : Name_Id;
      Nam2          : Name_Id;
      Check_Parents : Boolean := True) return Boolean
   is
   begin
      return Present (Get_Rep_Item (E, Nam1, Nam2, Check_Parents));
   end Has_Rep_Item;

   function Has_Rep_Item (E : Entity_Id; N : Node_Id) return Boolean is
      Item : Node_Id;

   begin
      pragma Assert
        (Nkind_In (N, N_Aspect_Specification,
                      N_Attribute_Definition_Clause,
                      N_Enumeration_Representation_Clause,
                      N_Pragma,
                      N_Record_Representation_Clause));

      Item := First_Rep_Item (E);
      while Present (Item) loop
         if Item = N then
            return True;
         end if;

         Item := Next_Rep_Item (Item);
      end loop;

      return False;
   end Has_Rep_Item;

   --------------------
   -- Has_Rep_Pragma --
   --------------------

   function Has_Rep_Pragma
     (E             : Entity_Id;
      Nam           : Name_Id;
      Check_Parents : Boolean := True) return Boolean
   is
   begin
      return Present (Get_Rep_Pragma (E, Nam, Check_Parents));
   end Has_Rep_Pragma;

   function Has_Rep_Pragma
     (E             : Entity_Id;
      Nam1          : Name_Id;
      Nam2          : Name_Id;
      Check_Parents : Boolean := True) return Boolean
   is
   begin
      return Present (Get_Rep_Pragma (E, Nam1, Nam2, Check_Parents));
   end Has_Rep_Pragma;

   --------------------------------
   -- Has_Unconstrained_Elements --
   --------------------------------

   function Has_Unconstrained_Elements (T : Entity_Id) return Boolean is
      U_T : constant Entity_Id := Underlying_Type (T);
   begin
      if No (U_T) then
         return False;
      elsif Is_Record_Type (U_T) then
         return Has_Discriminants (U_T) and then not Is_Constrained (U_T);
      elsif Is_Array_Type (U_T) then
         return Has_Unconstrained_Elements (Component_Type (U_T));
      else
         return False;
      end if;
   end Has_Unconstrained_Elements;

   ----------------------
   -- Has_Variant_Part --
   ----------------------

   function Has_Variant_Part (Typ : Entity_Id) return Boolean is
      FSTyp : Entity_Id;
      Decl  : Node_Id;
      TDef  : Node_Id;
      CList : Node_Id;

   begin
      if not Is_Type (Typ) then
         return False;
      end if;

      FSTyp := First_Subtype (Typ);

      if not Has_Discriminants (FSTyp) then
         return False;
      end if;

      --  Proceed with cautious checks here, return False if tree is not
      --  as expected (may be caused by prior errors).

      Decl := Declaration_Node (FSTyp);

      if Nkind (Decl) /= N_Full_Type_Declaration then
         return False;
      end if;

      TDef := Type_Definition (Decl);

      if Nkind (TDef) /= N_Record_Definition then
         return False;
      end if;

      CList := Component_List (TDef);

      if Nkind (CList) /= N_Component_List then
         return False;
      else
         return Present (Variant_Part (CList));
      end if;
   end Has_Variant_Part;

   ---------------------
   -- In_Generic_Body --
   ---------------------

   function In_Generic_Body (Id : Entity_Id) return Boolean is
      S : Entity_Id;

   begin
      --  Climb scopes looking for generic body

      S := Id;
      while Present (S) and then S /= Standard_Standard loop

         --  Generic package body

         if Ekind (S) = E_Generic_Package
           and then In_Package_Body (S)
         then
            return True;

         --  Generic subprogram body

         elsif Is_Subprogram (S)
           and then Nkind (Unit_Declaration_Node (S)) =
                      N_Generic_Subprogram_Declaration
         then
            return True;
         end if;

         S := Scope (S);
      end loop;

      --  False if top of scope stack without finding a generic body

      return False;
   end In_Generic_Body;

   -------------------------------
   -- Initialization_Suppressed --
   -------------------------------

   function Initialization_Suppressed (Typ : Entity_Id) return Boolean is
   begin
      return Suppress_Initialization (Typ)
        or else Suppress_Initialization (Base_Type (Typ));
   end Initialization_Suppressed;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Obsolescent_Warnings.Init;
   end Initialize;

   -------------
   -- Is_Body --
   -------------

   function Is_Body (N : Node_Id) return Boolean is
   begin
      return
        Nkind (N) in N_Body_Stub
          or else Nkind_In (N, N_Entry_Body,
                               N_Package_Body,
                               N_Protected_Body,
                               N_Subprogram_Body,
                               N_Task_Body);
   end Is_Body;

   ---------------------
   -- Is_By_Copy_Type --
   ---------------------

   function Is_By_Copy_Type (Ent : Entity_Id) return Boolean is
   begin
      --  If Id is a private type whose full declaration has not been seen,
      --  we assume for now that it is not a By_Copy type. Clearly this
      --  attribute should not be used before the type is frozen, but it is
      --  needed to build the associated record of a protected type. Another
      --  place where some lookahead for a full view is needed ???

      return
        Is_Elementary_Type (Ent)
          or else (Is_Private_Type (Ent)
                     and then Present (Underlying_Type (Ent))
                     and then Is_Elementary_Type (Underlying_Type (Ent)));
   end Is_By_Copy_Type;

   --------------------------
   -- Is_By_Reference_Type --
   --------------------------

   function Is_By_Reference_Type (Ent : Entity_Id) return Boolean is
      Btype : constant Entity_Id := Base_Type (Ent);

   begin
      if Error_Posted (Ent) or else Error_Posted (Btype) then
         return False;

      elsif Is_Private_Type (Btype) then
         declare
            Utyp : constant Entity_Id := Underlying_Type (Btype);
         begin
            if No (Utyp) then
               return False;
            else
               return Is_By_Reference_Type (Utyp);
            end if;
         end;

      elsif Is_Incomplete_Type (Btype) then
         declare
            Ftyp : constant Entity_Id := Full_View (Btype);
         begin
            --  Return true for a tagged incomplete type built as a shadow
            --  entity in Build_Limited_Views. It can appear in the profile
            --  of a thunk and the back end needs to know how it is passed.

            if No (Ftyp) then
               return Is_Tagged_Type (Btype);
            else
               return Is_By_Reference_Type (Ftyp);
            end if;
         end;

      elsif Is_Concurrent_Type (Btype) then
         return True;

      elsif Is_Record_Type (Btype) then
         if Is_Limited_Record (Btype)
           or else Is_Tagged_Type (Btype)
           or else Is_Volatile (Btype)
         then
            return True;

         else
            declare
               C : Entity_Id;

            begin
               C := First_Component (Btype);
               while Present (C) loop

                  --  For each component, test if its type is a by reference
                  --  type and if its type is volatile. Also test the component
                  --  itself for being volatile. This happens for example when
                  --  a Volatile aspect is added to a component.

                  if Is_By_Reference_Type (Etype (C))
                    or else Is_Volatile (Etype (C))
                    or else Is_Volatile (C)
                  then
                     return True;
                  end if;

                  C := Next_Component (C);
               end loop;
            end;

            return False;
         end if;

      elsif Is_Array_Type (Btype) then
         return
           Is_Volatile (Btype)
             or else Is_By_Reference_Type (Component_Type (Btype))
             or else Is_Volatile (Component_Type (Btype))
             or else Has_Volatile_Components (Btype);

      else
         return False;
      end if;
   end Is_By_Reference_Type;

   -------------------------
   -- Is_Definite_Subtype --
   -------------------------

   function Is_Definite_Subtype (T : Entity_Id) return Boolean is
      pragma Assert (Is_Type (T));
      K : constant Entity_Kind := Ekind (T);

   begin
      if Is_Constrained (T) then
         return True;

      elsif K in Array_Kind
        or else K in Class_Wide_Kind
        or else Has_Unknown_Discriminants (T)
      then
         return False;

      --  Known discriminants: definite if there are default values. Note that
      --  if any discriminant has a default, they all do.

      elsif Has_Discriminants (T) then
         return Present (Discriminant_Default_Value (First_Discriminant (T)));

      else
         return True;
      end if;
   end Is_Definite_Subtype;

   ---------------------
   -- Is_Derived_Type --
   ---------------------

   function Is_Derived_Type (Ent : E) return B is
      Par : Node_Id;

   begin
      if Is_Type (Ent)
        and then Base_Type (Ent) /= Root_Type (Ent)
        and then not Is_Class_Wide_Type (Ent)

        --  An access_to_subprogram whose result type is a limited view can
        --  appear in a return statement, without the full view of the result
        --  type being available. Do not interpret this as a derived type.

        and then Ekind (Ent) /= E_Subprogram_Type
      then
         if not Is_Numeric_Type (Root_Type (Ent)) then
            return True;

         else
            Par := Parent (First_Subtype (Ent));

            return Present (Par)
              and then Nkind (Par) = N_Full_Type_Declaration
              and then Nkind (Type_Definition (Par)) =
                         N_Derived_Type_Definition;
         end if;

      else
         return False;
      end if;
   end Is_Derived_Type;

   -----------------------
   -- Is_Generic_Formal --
   -----------------------

   function Is_Generic_Formal (E : Entity_Id) return Boolean is
      Kind : Node_Kind;

   begin
      if No (E) then
         return False;
      else
         --  Formal derived types are rewritten as private extensions, so
         --  examine original node.

         Kind := Nkind (Original_Node (Parent (E)));

         return
           Nkind_In (Kind, N_Formal_Object_Declaration,
                           N_Formal_Type_Declaration)
             or else Is_Formal_Subprogram (E)
             or else
               (Ekind (E) = E_Package
                 and then Nkind (Original_Node (Unit_Declaration_Node (E))) =
                            N_Formal_Package_Declaration);
      end if;
   end Is_Generic_Formal;

   -------------------------------
   -- Is_Immutably_Limited_Type --
   -------------------------------

   function Is_Immutably_Limited_Type (Ent : Entity_Id) return Boolean is
      Btype : constant Entity_Id := Available_View (Base_Type (Ent));

   begin
      if Is_Limited_Record (Btype) then
         return True;

      elsif Ekind (Btype) = E_Limited_Private_Type
        and then Nkind (Parent (Btype)) = N_Formal_Type_Declaration
      then
         return not In_Package_Body (Scope ((Btype)));

      elsif Is_Private_Type (Btype) then

         --  AI05-0063: A type derived from a limited private formal type is
         --  not immutably limited in a generic body.

         if Is_Derived_Type (Btype)
           and then Is_Generic_Type (Etype (Btype))
         then
            if not Is_Limited_Type (Etype (Btype)) then
               return False;

            --  A descendant of a limited formal type is not immutably limited
            --  in the generic body, or in the body of a generic child.

            elsif Ekind (Scope (Etype (Btype))) = E_Generic_Package then
               return not In_Package_Body (Scope (Btype));

            else
               return False;
            end if;

         else
            declare
               Utyp : constant Entity_Id := Underlying_Type (Btype);
            begin
               if No (Utyp) then
                  return False;
               else
                  return Is_Immutably_Limited_Type (Utyp);
               end if;
            end;
         end if;

      elsif Is_Concurrent_Type (Btype) then
         return True;

      else
         return False;
      end if;
   end Is_Immutably_Limited_Type;

   ---------------------
   -- Is_Limited_Type --
   ---------------------

   function Is_Limited_Type (Ent : Entity_Id) return Boolean is
      Btype : constant E := Base_Type (Ent);
      Rtype : constant E := Root_Type (Btype);

   begin
      if not Is_Type (Ent) then
         return False;

      elsif Ekind (Btype) = E_Limited_Private_Type
        or else Is_Limited_Composite (Btype)
      then
         return True;

      elsif Is_Concurrent_Type (Btype) then
         return True;

         --  The Is_Limited_Record flag normally indicates that the type is
         --  limited. The exception is that a type does not inherit limitedness
         --  from its interface ancestor. So the type may be derived from a
         --  limited interface, but is not limited.

      elsif Is_Limited_Record (Ent)
        and then not Is_Interface (Ent)
      then
         return True;

      --  Otherwise we will look around to see if there is some other reason
      --  for it to be limited, except that if an error was posted on the
      --  entity, then just assume it is non-limited, because it can cause
      --  trouble to recurse into a murky entity resulting from other errors.

      elsif Error_Posted (Ent) then
         return False;

      elsif Is_Record_Type (Btype) then

         if Is_Limited_Interface (Ent) then
            return True;

         --  AI-419: limitedness is not inherited from a limited interface

         elsif Is_Limited_Record (Rtype) then
            return not Is_Interface (Rtype)
              or else Is_Protected_Interface (Rtype)
              or else Is_Synchronized_Interface (Rtype)
              or else Is_Task_Interface (Rtype);

         elsif Is_Class_Wide_Type (Btype) then
            return Is_Limited_Type (Rtype);

         else
            declare
               C : E;

            begin
               C := First_Component (Btype);
               while Present (C) loop
                  if Is_Limited_Type (Etype (C)) then
                     return True;
                  end if;

                  C := Next_Component (C);
               end loop;
            end;

            return False;
         end if;

      elsif Is_Array_Type (Btype) then
         return Is_Limited_Type (Component_Type (Btype));

      else
         return False;
      end if;
   end Is_Limited_Type;

   ---------------------
   -- Is_Limited_View --
   ---------------------

   function Is_Limited_View (Ent : Entity_Id) return Boolean is
      Btype : constant Entity_Id := Available_View (Base_Type (Ent));

   begin
      if Is_Limited_Record (Btype) then
         return True;

      elsif Ekind (Btype) = E_Limited_Private_Type
        and then Nkind (Parent (Btype)) = N_Formal_Type_Declaration
      then
         return not In_Package_Body (Scope ((Btype)));

      elsif Is_Private_Type (Btype) then

         --  AI05-0063: A type derived from a limited private formal type is
         --  not immutably limited in a generic body.

         if Is_Derived_Type (Btype)
           and then Is_Generic_Type (Etype (Btype))
         then
            if not Is_Limited_Type (Etype (Btype)) then
               return False;

            --  A descendant of a limited formal type is not immutably limited
            --  in the generic body, or in the body of a generic child.

            elsif Ekind (Scope (Etype (Btype))) = E_Generic_Package then
               return not In_Package_Body (Scope (Btype));

            else
               return False;
            end if;

         else
            declare
               Utyp : constant Entity_Id := Underlying_Type (Btype);
            begin
               if No (Utyp) then
                  return False;
               else
                  return Is_Limited_View (Utyp);
               end if;
            end;
         end if;

      elsif Is_Concurrent_Type (Btype) then
         return True;

      elsif Is_Record_Type (Btype) then

         --  Note that we return True for all limited interfaces, even though
         --  (unsynchronized) limited interfaces can have descendants that are
         --  nonlimited, because this is a predicate on the type itself, and
         --  things like functions with limited interface results need to be
         --  handled as build in place even though they might return objects
         --  of a type that is not inherently limited.

         if Is_Class_Wide_Type (Btype) then
            return Is_Limited_View (Root_Type (Btype));

         else
            declare
               C : Entity_Id;

            begin
               C := First_Component (Btype);
               while Present (C) loop

                  --  Don't consider components with interface types (which can
                  --  only occur in the case of a _parent component anyway).
                  --  They don't have any components, plus it would cause this
                  --  function to return true for nonlimited types derived from
                  --  limited interfaces.

                  if not Is_Interface (Etype (C))
                    and then Is_Limited_View (Etype (C))
                  then
                     return True;
                  end if;

                  C := Next_Component (C);
               end loop;
            end;

            return False;
         end if;

      elsif Is_Array_Type (Btype) then
         return Is_Limited_View (Component_Type (Btype));

      else
         return False;
      end if;
   end Is_Limited_View;

   ----------------------------
   -- Is_Protected_Operation --
   ----------------------------

   function Is_Protected_Operation (E : Entity_Id) return Boolean is
   begin
      return
        Is_Entry (E)
          or else (Is_Subprogram (E)
                    and then Nkind (Parent (Unit_Declaration_Node (E))) =
                               N_Protected_Definition);
   end Is_Protected_Operation;

   ----------------------
   -- Nearest_Ancestor --
   ----------------------

   function Nearest_Ancestor (Typ : Entity_Id) return Entity_Id is
      D : constant Node_Id := Original_Node (Declaration_Node (Typ));
      --  We use the original node of the declaration, because derived
      --  types from record subtypes are rewritten as record declarations,
      --  and it is the original declaration that carries the ancestor.

   begin
      --  If we have a subtype declaration, get the ancestor subtype

      if Nkind (D) = N_Subtype_Declaration then
         if Nkind (Subtype_Indication (D)) = N_Subtype_Indication then
            return Entity (Subtype_Mark (Subtype_Indication (D)));
         else
            return Entity (Subtype_Indication (D));
         end if;

      --  If derived type declaration, find who we are derived from

      elsif Nkind (D) = N_Full_Type_Declaration
        and then Nkind (Type_Definition (D)) = N_Derived_Type_Definition
      then
         declare
            DTD : constant Entity_Id := Type_Definition (D);
            SI  : constant Entity_Id := Subtype_Indication (DTD);
         begin
            if Is_Entity_Name (SI) then
               return Entity (SI);
            else
               return Entity (Subtype_Mark (SI));
            end if;
         end;

      --  If derived type and private type, get the full view to find who we
      --  are derived from.

      elsif Is_Derived_Type (Typ)
        and then Is_Private_Type (Typ)
        and then Present (Full_View (Typ))
      then
         return Nearest_Ancestor (Full_View (Typ));

      --  Otherwise, nothing useful to return, return Empty

      else
         return Empty;
      end if;
   end Nearest_Ancestor;

   ---------------------------
   -- Nearest_Dynamic_Scope --
   ---------------------------

   function Nearest_Dynamic_Scope (Ent : Entity_Id) return Entity_Id is
   begin
      if Is_Dynamic_Scope (Ent) then
         return Ent;
      else
         return Enclosing_Dynamic_Scope (Ent);
      end if;
   end Nearest_Dynamic_Scope;

   ------------------------
   -- Next_Tag_Component --
   ------------------------

   function Next_Tag_Component (Tag : Entity_Id) return Entity_Id is
      Comp : Entity_Id;

   begin
      pragma Assert (Is_Tag (Tag));

      --  Loop to look for next tag component

      Comp := Next_Entity (Tag);
      while Present (Comp) loop
         if Is_Tag (Comp) then
            pragma Assert (Chars (Comp) /= Name_uTag);
            return Comp;
         end if;

         Comp := Next_Entity (Comp);
      end loop;

      --  No tag component found

      return Empty;
   end Next_Tag_Component;

   -----------------------
   -- Number_Components --
   -----------------------

   function Number_Components (Typ : Entity_Id) return Nat is
      N    : Nat := 0;
      Comp : Entity_Id;

   begin
      --  We do not call Einfo.First_Component_Or_Discriminant, as this
      --  function does not skip completely hidden discriminants, which we
      --  want to skip here.

      if Has_Discriminants (Typ) then
         Comp := First_Discriminant (Typ);
      else
         Comp := First_Component (Typ);
      end if;

      while Present (Comp) loop
         N := N + 1;
         Comp := Next_Component_Or_Discriminant (Comp);
      end loop;

      return N;
   end Number_Components;

   --------------------------
   -- Number_Discriminants --
   --------------------------

   function Number_Discriminants (Typ : Entity_Id) return Pos is
      N     : Nat       := 0;
      Discr : Entity_Id := First_Discriminant (Typ);

   begin
      while Present (Discr) loop
         N := N + 1;
         Discr := Next_Discriminant (Discr);
      end loop;

      return N;
   end Number_Discriminants;

   ----------------------------------------------
   -- Object_Type_Has_Constrained_Partial_View --
   ----------------------------------------------

   function Object_Type_Has_Constrained_Partial_View
     (Typ  : Entity_Id;
      Scop : Entity_Id) return Boolean
   is
   begin
      return Has_Constrained_Partial_View (Typ)
        or else (In_Generic_Body (Scop)
                  and then Is_Generic_Type (Base_Type (Typ))
                  and then (Is_Private_Type (Base_Type (Typ))
                             or else Is_Derived_Type (Base_Type (Typ)))
                  and then not Is_Tagged_Type (Typ)
                  and then not (Is_Array_Type (Typ)
                                 and then not Is_Constrained (Typ))
                  and then Has_Discriminants (Typ));
   end Object_Type_Has_Constrained_Partial_View;

   ------------------
   -- Package_Body --
   ------------------

   function Package_Body (E : Entity_Id) return Node_Id is
      N : Node_Id;

   begin
      if Ekind (E) = E_Package_Body then
         N := Parent (E);

         if Nkind (N) = N_Defining_Program_Unit_Name then
            N := Parent (N);
         end if;

      else
         N := Package_Spec (E);

         if Present (Corresponding_Body (N)) then
            N := Parent (Corresponding_Body (N));

            if Nkind (N) = N_Defining_Program_Unit_Name then
               N := Parent (N);
            end if;
         else
            N := Empty;
         end if;
      end if;

      return N;
   end Package_Body;

   ------------------
   -- Package_Spec --
   ------------------

   function Package_Spec (E : Entity_Id) return Node_Id is
   begin
      return Parent (Package_Specification (E));
   end Package_Spec;

   ---------------------------
   -- Package_Specification --
   ---------------------------

   function Package_Specification (E : Entity_Id) return Node_Id is
      N : Node_Id;

   begin
      N := Parent (E);

      if Nkind (N) = N_Defining_Program_Unit_Name then
         N := Parent (N);
      end if;

      return N;
   end Package_Specification;

   ---------------------
   -- Subprogram_Body --
   ---------------------

   function Subprogram_Body (E : Entity_Id) return Node_Id is
      Body_E : constant Entity_Id := Subprogram_Body_Entity (E);

   begin
      if No (Body_E) then
         return Empty;
      else
         return Parent (Subprogram_Specification (Body_E));
      end if;
   end Subprogram_Body;

   ----------------------------
   -- Subprogram_Body_Entity --
   ----------------------------

   function Subprogram_Body_Entity (E : Entity_Id) return Entity_Id is
      N : constant Node_Id := Parent (Subprogram_Specification (E));
      --  Declaration for E

   begin
      --  If this declaration is not a subprogram body, then it must be a
      --  subprogram declaration or body stub, from which we can retrieve the
      --  entity for the corresponding subprogram body if any, or an abstract
      --  subprogram declaration, for which we return Empty.

      case Nkind (N) is
         when N_Subprogram_Body =>
            return E;

         when N_Subprogram_Body_Stub
            | N_Subprogram_Declaration
         =>
            return Corresponding_Body (N);

         when others =>
            return Empty;
      end case;
   end Subprogram_Body_Entity;

   ---------------------
   -- Subprogram_Spec --
   ---------------------

   function Subprogram_Spec (E : Entity_Id) return Node_Id is
      N : constant Node_Id := Parent (Subprogram_Specification (E));
      --  Declaration for E

   begin
      --  This declaration is either subprogram declaration or a subprogram
      --  body, in which case return Empty.

      if Nkind (N) = N_Subprogram_Declaration then
         return N;
      else
         return Empty;
      end if;
   end Subprogram_Spec;

   ------------------------------
   -- Subprogram_Specification --
   ------------------------------

   function Subprogram_Specification (E : Entity_Id) return Node_Id is
      N : Node_Id;

   begin
      N := Parent (E);

      if Nkind (N) = N_Defining_Program_Unit_Name then
         N := Parent (N);
      end if;

      --  If the Parent pointer of E is not a subprogram specification node
      --  (going through an intermediate N_Defining_Program_Unit_Name node
      --  for subprogram units), then E is an inherited operation. Its parent
      --  points to the type derivation that produces the inheritance: that's
      --  the node that generates the subprogram specification. Its alias
      --  is the parent subprogram, and that one points to a subprogram
      --  declaration, or to another type declaration if this is a hierarchy
      --  of derivations.

      if Nkind (N) not in N_Subprogram_Specification then
         pragma Assert (Present (Alias (E)));
         N := Subprogram_Specification (Alias (E));
      end if;

      return N;
   end Subprogram_Specification;

   ---------------
   -- Tree_Read --
   ---------------

   procedure Tree_Read is
   begin
      Obsolescent_Warnings.Tree_Read;
   end Tree_Read;

   ----------------
   -- Tree_Write --
   ----------------

   procedure Tree_Write is
   begin
      Obsolescent_Warnings.Tree_Write;
   end Tree_Write;

   --------------------
   -- Ultimate_Alias --
   --------------------

   function Ultimate_Alias (Prim : Entity_Id) return Entity_Id is
      E : Entity_Id := Prim;

   begin
      while Present (Alias (E)) loop
         pragma Assert (Alias (E) /= E);
         E := Alias (E);
      end loop;

      return E;
   end Ultimate_Alias;

   --------------------------
   -- Unit_Declaration_Node --
   --------------------------

   function Unit_Declaration_Node (Unit_Id : Entity_Id) return Node_Id is
      N : Node_Id := Parent (Unit_Id);

   begin
      --  Predefined operators do not have a full function declaration

      if Ekind (Unit_Id) = E_Operator then
         return N;
      end if;

      --  Isn't there some better way to express the following ???

      while Nkind (N) /= N_Abstract_Subprogram_Declaration
        and then Nkind (N) /= N_Entry_Body
        and then Nkind (N) /= N_Entry_Declaration
        and then Nkind (N) /= N_Formal_Package_Declaration
        and then Nkind (N) /= N_Function_Instantiation
        and then Nkind (N) /= N_Generic_Package_Declaration
        and then Nkind (N) /= N_Generic_Subprogram_Declaration
        and then Nkind (N) /= N_Package_Declaration
        and then Nkind (N) /= N_Package_Body
        and then Nkind (N) /= N_Package_Instantiation
        and then Nkind (N) /= N_Package_Renaming_Declaration
        and then Nkind (N) /= N_Procedure_Instantiation
        and then Nkind (N) /= N_Protected_Body
        and then Nkind (N) /= N_Protected_Type_Declaration
        and then Nkind (N) /= N_Subprogram_Declaration
        and then Nkind (N) /= N_Subprogram_Body
        and then Nkind (N) /= N_Subprogram_Body_Stub
        and then Nkind (N) /= N_Subprogram_Renaming_Declaration
        and then Nkind (N) /= N_Task_Body
        and then Nkind (N) /= N_Task_Type_Declaration
        and then Nkind (N) not in N_Formal_Subprogram_Declaration
        and then Nkind (N) not in N_Generic_Renaming_Declaration
      loop
         N := Parent (N);

         --  We don't use Assert here, because that causes an infinite loop
         --  when assertions are turned off. Better to crash.

         if No (N) then
            raise Program_Error;
         end if;
      end loop;

      return N;
   end Unit_Declaration_Node;

end Sem_Aux;
