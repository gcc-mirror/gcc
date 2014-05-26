------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             L I B . X R E F                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1998-2014, Free Software Foundation, Inc.         --
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
with Csets;    use Csets;
with Elists;   use Elists;
with Errout;   use Errout;
with Nlists;   use Nlists;
with Opt;      use Opt;
with Restrict; use Restrict;
with Rident;   use Rident;
with Sem;      use Sem;
with Sem_Aux;  use Sem_Aux;
with Sem_Prag; use Sem_Prag;
with Sem_Util; use Sem_Util;
with Sem_Warn; use Sem_Warn;
with Sinfo;    use Sinfo;
with Sinput;   use Sinput;
with Snames;   use Snames;
with Stringt;  use Stringt;
with Stand;    use Stand;
with Table;    use Table;

with GNAT.Heap_Sort_G;
with GNAT.HTable;

package body Lib.Xref is

   ------------------
   -- Declarations --
   ------------------

   --  The Xref table is used to record references. The Loc field is set
   --  to No_Location for a definition entry.

   subtype Xref_Entry_Number is Int;

   type Xref_Key is record
      --  These are the components of Xref_Entry that participate in hash
      --  lookups.

      Ent : Entity_Id;
      --  Entity referenced (E parameter to Generate_Reference)

      Loc : Source_Ptr;
      --  Location of reference (Original_Location (Sloc field of N parameter
      --  to Generate_Reference). Set to No_Location for the case of a
      --  defining occurrence.

      Typ : Character;
      --  Reference type (Typ param to Generate_Reference)

      Eun : Unit_Number_Type;
      --  Unit number corresponding to Ent

      Lun : Unit_Number_Type;
      --  Unit number corresponding to Loc. Value is undefined and not
      --  referenced if Loc is set to No_Location.

      --  The following components are only used for SPARK cross-references

      Ref_Scope : Entity_Id;
      --  Entity of the closest subprogram or package enclosing the reference

      Ent_Scope : Entity_Id;
      --  Entity of the closest subprogram or package enclosing the definition,
      --  which should be located in the same file as the definition itself.
   end record;

   type Xref_Entry is record
      Key : Xref_Key;

      Ent_Scope_File : Unit_Number_Type;
      --  File for entity Ent_Scope

      Def : Source_Ptr;
      --  Original source location for entity being referenced. Note that these
      --  values are used only during the output process, they are not set when
      --  the entries are originally built. This is because private entities
      --  can be swapped when the initial call is made.

      HTable_Next : Xref_Entry_Number;
      --  For use only by Static_HTable
   end record;

   package Xrefs is new Table.Table (
     Table_Component_Type => Xref_Entry,
     Table_Index_Type     => Xref_Entry_Number,
     Table_Low_Bound      => 1,
     Table_Initial        => Alloc.Xrefs_Initial,
     Table_Increment      => Alloc.Xrefs_Increment,
     Table_Name           => "Xrefs");

   --------------
   -- Xref_Set --
   --------------

   --  We keep a set of xref entries, in order to avoid inserting duplicate
   --  entries into the above Xrefs table. An entry is in Xref_Set if and only
   --  if it is in Xrefs.

   Num_Buckets : constant := 2**16;

   subtype Header_Num is Integer range 0 .. Num_Buckets - 1;
   type Null_Type is null record;
   pragma Unreferenced (Null_Type);

   function Hash (F : Xref_Entry_Number) return Header_Num;

   function Equal (F1, F2 : Xref_Entry_Number) return Boolean;

   procedure HT_Set_Next (E : Xref_Entry_Number; Next : Xref_Entry_Number);

   function  HT_Next (E : Xref_Entry_Number) return Xref_Entry_Number;

   function Get_Key (E : Xref_Entry_Number) return Xref_Entry_Number;

   pragma Inline (Hash, Equal, HT_Set_Next, HT_Next, Get_Key);

   package Xref_Set is new GNAT.HTable.Static_HTable (
     Header_Num,
     Element    => Xref_Entry,
     Elmt_Ptr   => Xref_Entry_Number,
     Null_Ptr   => 0,
     Set_Next   => HT_Set_Next,
     Next       => HT_Next,
     Key        => Xref_Entry_Number,
     Get_Key    => Get_Key,
     Hash       => Hash,
     Equal      => Equal);

   -----------------------------
   -- SPARK Xrefs Information --
   -----------------------------

   package body SPARK_Specific is separate;

   ------------------------
   --  Local Subprograms --
   ------------------------

   procedure Add_Entry (Key : Xref_Key; Ent_Scope_File : Unit_Number_Type);
   --  Add an entry to the tables of Xref_Entries, avoiding duplicates

   procedure Generate_Prim_Op_References (Typ : Entity_Id);
   --  For a tagged type, generate implicit references to its primitive
   --  operations, for source navigation. This is done right before emitting
   --  cross-reference information rather than at the freeze point of the type
   --  in order to handle late bodies that are primitive operations.

   function Lt (T1, T2 : Xref_Entry) return Boolean;
   --  Order cross-references

   ---------------
   -- Add_Entry --
   ---------------

   procedure Add_Entry (Key : Xref_Key; Ent_Scope_File : Unit_Number_Type) is
   begin
      Xrefs.Increment_Last; -- tentative
      Xrefs.Table (Xrefs.Last).Key := Key;

      --  Set the entry in Xref_Set, and if newly set, keep the above
      --  tentative increment.

      if Xref_Set.Set_If_Not_Present (Xrefs.Last) then
         Xrefs.Table (Xrefs.Last).Ent_Scope_File := Ent_Scope_File;
         --  Leave Def and HTable_Next uninitialized

         Set_Has_Xref_Entry (Key.Ent);

      --  It was already in Xref_Set, so throw away the tentatively-added
      --  entry

      else
         Xrefs.Decrement_Last;
      end if;
   end Add_Entry;

   -----------
   -- Equal --
   -----------

   function Equal (F1, F2 : Xref_Entry_Number) return Boolean is
      Result : constant Boolean :=
                 Xrefs.Table (F1).Key = Xrefs.Table (F2).Key;
   begin
      return Result;
   end Equal;

   -------------------------
   -- Generate_Definition --
   -------------------------

   procedure Generate_Definition (E : Entity_Id) is
   begin
      pragma Assert (Nkind (E) in N_Entity);

      --  Note that we do not test Xref_Entity_Letters here. It is too early
      --  to do so, since we are often called before the entity is fully
      --  constructed, so that the Ekind is still E_Void.

      if Opt.Xref_Active

         --  Definition must come from source

         --  We make an exception for subprogram child units that have no spec.
         --  For these we generate a subprogram declaration for library use,
         --  and the corresponding entity does not come from source.
         --  Nevertheless, all references will be attached to it and we have
         --  to treat is as coming from user code.

         and then (Comes_From_Source (E) or else Is_Child_Unit (E))

         --  And must have a reasonable source location that is not
         --  within an instance (all entities in instances are ignored)

         and then Sloc (E) > No_Location
         and then Instantiation_Location (Sloc (E)) = No_Location

         --  And must be a non-internal name from the main source unit

         and then In_Extended_Main_Source_Unit (E)
         and then not Is_Internal_Name (Chars (E))
      then
         Add_Entry
           ((Ent => E,
             Loc => No_Location,
             Typ => ' ',
             Eun => Get_Source_Unit (Original_Location (Sloc (E))),
             Lun => No_Unit,
             Ref_Scope => Empty,
             Ent_Scope => Empty),
            Ent_Scope_File => No_Unit);

         if In_Inlined_Body then
            Set_Referenced (E);
         end if;
      end if;
   end Generate_Definition;

   ---------------------------------
   -- Generate_Operator_Reference --
   ---------------------------------

   procedure Generate_Operator_Reference
     (N : Node_Id;
      T : Entity_Id)
   is
   begin
      if not In_Extended_Main_Source_Unit (N) then
         return;
      end if;

      --  If the operator is not a Standard operator, then we generate a real
      --  reference to the user defined operator.

      if Sloc (Entity (N)) /= Standard_Location then
         Generate_Reference (Entity (N), N);

         --  A reference to an implicit inequality operator is also a reference
         --  to the user-defined equality.

         if Nkind (N) = N_Op_Ne
           and then not Comes_From_Source (Entity (N))
           and then Present (Corresponding_Equality (Entity (N)))
         then
            Generate_Reference (Corresponding_Equality (Entity (N)), N);
         end if;

      --  For the case of Standard operators, we mark the result type as
      --  referenced. This ensures that in the case where we are using a
      --  derived operator, we mark an entity of the unit that implicitly
      --  defines this operator as used. Otherwise we may think that no entity
      --  of the unit is used. The actual entity marked as referenced is the
      --  first subtype, which is the relevant user defined entity.

      --  Note: we only do this for operators that come from source. The
      --  generated code sometimes reaches for entities that do not need to be
      --  explicitly visible (for example, when we expand the code for
      --  comparing two record objects, the fields of the record may not be
      --  visible).

      elsif Comes_From_Source (N) then
         Set_Referenced (First_Subtype (T));
      end if;
   end Generate_Operator_Reference;

   ---------------------------------
   -- Generate_Prim_Op_References --
   ---------------------------------

   procedure Generate_Prim_Op_References (Typ : Entity_Id) is
      Base_T    : Entity_Id;
      Prim      : Elmt_Id;
      Prim_List : Elist_Id;

   begin
      --  Handle subtypes of synchronized types

      if Ekind (Typ) = E_Protected_Subtype
        or else Ekind (Typ) = E_Task_Subtype
      then
         Base_T := Etype (Typ);
      else
         Base_T := Typ;
      end if;

      --  References to primitive operations are only relevant for tagged types

      if not Is_Tagged_Type (Base_T)
        or else Is_Class_Wide_Type (Base_T)
      then
         return;
      end if;

      --  Ada 2005 (AI-345): For synchronized types generate reference to the
      --  wrapper that allow us to dispatch calls through their implemented
      --  abstract interface types.

      --  The check for Present here is to protect against previously reported
      --  critical errors.

      Prim_List := Primitive_Operations (Base_T);

      if No (Prim_List) then
         return;
      end if;

      Prim := First_Elmt (Prim_List);
      while Present (Prim) loop

         --  If the operation is derived, get the original for cross-reference
         --  reference purposes (it is the original for which we want the xref
         --  and for which the comes_from_source test must be performed).

         Generate_Reference
           (Typ, Ultimate_Alias (Node (Prim)), 'p', Set_Ref => False);
         Next_Elmt (Prim);
      end loop;
   end Generate_Prim_Op_References;

   ------------------------
   -- Generate_Reference --
   ------------------------

   procedure Generate_Reference
     (E       : Entity_Id;
      N       : Node_Id;
      Typ     : Character := 'r';
      Set_Ref : Boolean   := True;
      Force   : Boolean   := False)
   is
      Actual_Typ     : Character := Typ;
      Call           : Node_Id;
      Def            : Source_Ptr;
      Ent            : Entity_Id;
      Ent_Scope      : Entity_Id;
      Formal         : Entity_Id;
      Kind           : Entity_Kind;
      Nod            : Node_Id;
      Ref            : Source_Ptr;
      Ref_Scope      : Entity_Id;

      function Get_Through_Renamings (E : Entity_Id) return Entity_Id;
      --  Get the enclosing entity through renamings, which may come from
      --  source or from the translation of generic instantiations.

      function Is_On_LHS (Node : Node_Id) return Boolean;
      --  Used to check if a node is on the left hand side of an assignment.
      --  The following cases are handled:
      --
      --   Variable    Node is a direct descendant of left hand side of an
      --               assignment statement.
      --
      --   Prefix      Of an indexed or selected component that is present in
      --               a subtree rooted by an assignment statement. There is
      --               no restriction of nesting of components, thus cases
      --               such as A.B (C).D are handled properly. However a prefix
      --               of a dereference (either implicit or explicit) is never
      --               considered as on a LHS.
      --
      --   Out param   Same as above cases, but OUT parameter

      function OK_To_Set_Referenced return Boolean;
      --  Returns True if the Referenced flag can be set. There are a few
      --  exceptions where we do not want to set this flag, see body for
      --  details of these exceptional cases.

      ---------------------------
      -- Get_Through_Renamings --
      ---------------------------

      function Get_Through_Renamings (E : Entity_Id) return Entity_Id is
         Result : Entity_Id := E;
      begin
         while Present (Result)
           and then Is_Object (Result)
           and then Present (Renamed_Object (Result))
         loop
            Result := Get_Enclosing_Object (Renamed_Object (Result));
         end loop;
         return Result;
      end Get_Through_Renamings;

      ---------------
      -- Is_On_LHS --
      ---------------

      --  ??? There are several routines here and there that perform a similar
      --      (but subtly different) computation, which should be factored:

      --      Sem_Util.Is_LHS
      --      Sem_Util.May_Be_Lvalue
      --      Sem_Util.Known_To_Be_Assigned
      --      Exp_Ch2.Expand_Entry_Parameter.In_Assignment_Context
      --      Exp_Smem.Is_Out_Actual

      function Is_On_LHS (Node : Node_Id) return Boolean is
         N : Node_Id;
         P : Node_Id;
         K : Node_Kind;

      begin
         --  Only identifiers are considered, is this necessary???

         if Nkind (Node) /= N_Identifier then
            return False;
         end if;

         --  Immediate return if appeared as OUT parameter

         if Kind = E_Out_Parameter then
            return True;
         end if;

         --  Search for assignment statement subtree root

         N := Node;
         loop
            P := Parent (N);
            K := Nkind (P);

            if K = N_Assignment_Statement then
               return Name (P) = N;

            --  Check whether the parent is a component and the current node is
            --  its prefix, but return False if the current node has an access
            --  type, as in that case the selected or indexed component is an
            --  implicit dereference, and the LHS is the designated object, not
            --  the access object.

            --  ??? case of a slice assignment?

            elsif (K = N_Selected_Component or else K = N_Indexed_Component)
              and then Prefix (P) = N
            then
               --  Check for access type. First a kludge, In some cases this is
               --  called too early (see comments in Sem_Ch8.Find_Direct_Name),
               --  at a point where the tree is not fully typed yet. In that
               --  case we may lack an Etype for N, and we can't check the
               --  Etype. For now, we always return False in such a case,
               --  but this is clearly not right in all cases ???

               if No (Etype (N)) then
                  return False;

               elsif Is_Access_Type (Etype (N)) then
                  return False;

               --  Access type case dealt with, keep going

               else
                  N := P;
               end if;

            --  All other cases, definitely not on left side

            else
               return False;
            end if;
         end loop;
      end Is_On_LHS;

      ---------------------------
      -- OK_To_Set_Referenced --
      ---------------------------

      function OK_To_Set_Referenced return Boolean is
         P : Node_Id;

      begin
         --  A reference from a pragma Unreferenced or pragma Unmodified or
         --  pragma Warnings does not cause the Referenced flag to be set.
         --  This avoids silly warnings about things being referenced and
         --  not assigned when the only reference is from the pragma.

         if Nkind (N) = N_Identifier then
            P := Parent (N);

            if Nkind (P) = N_Pragma_Argument_Association then
               P := Parent (P);

               if Nkind (P) = N_Pragma then
                  if Nam_In (Pragma_Name (P), Name_Warnings,
                                              Name_Unmodified,
                                              Name_Unreferenced)
                  then
                     return False;
                  end if;
               end if;

            --  A reference to a formal in a named parameter association does
            --  not make the formal referenced. Formals that are unused in the
            --  subprogram body are properly flagged as such, even if calls
            --  elsewhere use named notation.

            elsif Nkind (P) = N_Parameter_Association
              and then N = Selector_Name (P)
            then
               return False;
            end if;
         end if;

         return True;
      end OK_To_Set_Referenced;

   --  Start of processing for Generate_Reference

   begin
      pragma Assert (Nkind (E) in N_Entity);
      Find_Actual (N, Formal, Call);

      if Present (Formal) then
         Kind := Ekind (Formal);
      else
         Kind := E_Void;
      end if;

      --  Check for obsolescent reference to package ASCII. GNAT treats this
      --  element of annex J specially since in practice, programs make a lot
      --  of use of this feature, so we don't include it in the set of features
      --  diagnosed when Warn_On_Obsolescent_Features mode is set. However we
      --  are required to note it as a violation of the RM defined restriction.

      if E = Standard_ASCII then
         Check_Restriction (No_Obsolescent_Features, N);
      end if;

      --  Check for reference to entity marked with Is_Obsolescent

      --  Note that we always allow obsolescent references in the compiler
      --  itself and the run time, since we assume that we know what we are
      --  doing in such cases. For example the calls in Ada.Characters.Handling
      --  to its own obsolescent subprograms are just fine.

      --  In any case we only generate warnings if we are in the extended main
      --  source unit, and the entity itself is not in the extended main source
      --  unit, since we assume the source unit itself knows what is going on
      --  (and for sure we do not want silly warnings, e.g. on the end line of
      --  an obsolescent procedure body).

      if Is_Obsolescent (E)
        and then not GNAT_Mode
        and then not In_Extended_Main_Source_Unit (E)
        and then In_Extended_Main_Source_Unit (N)
      then
         Check_Restriction (No_Obsolescent_Features, N);

         if Warn_On_Obsolescent_Feature then
            Output_Obsolescent_Entity_Warnings (N, E);
         end if;
      end if;

      --  Warn if reference to Ada 2005 entity not in Ada 2005 mode. We only
      --  detect real explicit references (modifications and references).

      if Comes_From_Source (N)
        and then Is_Ada_2005_Only (E)
        and then Ada_Version < Ada_2005
        and then Warn_On_Ada_2005_Compatibility
        and then (Typ = 'm' or else Typ = 'r' or else Typ = 's')
      then
         Error_Msg_NE ("& is only defined in Ada 2005?y?", N, E);
      end if;

      --  Warn if reference to Ada 2012 entity not in Ada 2012 mode. We only
      --  detect real explicit references (modifications and references).

      if Comes_From_Source (N)
        and then Is_Ada_2012_Only (E)
        and then Ada_Version < Ada_2012
        and then Warn_On_Ada_2012_Compatibility
        and then (Typ = 'm' or else Typ = 'r')
      then
         Error_Msg_NE ("& is only defined in Ada 2012?y?", N, E);
      end if;

      --  Do not generate references if we are within a postcondition sub-
      --  program, because the reference does not comes from source, and the
      --  pre-analysis of the aspect has already created an entry for the ali
      --  file at the proper source location.

      if Chars (Current_Scope) = Name_uPostconditions then
         return;
      end if;

      --  Never collect references if not in main source unit. However, we omit
      --  this test if Typ is 'e' or 'k', since these entries are structural,
      --  and it is useful to have them in units that reference packages as
      --  well as units that define packages. We also omit the test for the
      --  case of 'p' since we want to include inherited primitive operations
      --  from other packages.

      --  We also omit this test is this is a body reference for a subprogram
      --  instantiation. In this case the reference is to the generic body,
      --  which clearly need not be in the main unit containing the instance.
      --  For the same reason we accept an implicit reference generated for
      --  a default in an instance.

      if not In_Extended_Main_Source_Unit (N) then
         if Typ = 'e'
           or else Typ = 'I'
           or else Typ = 'p'
           or else Typ = 'i'
           or else Typ = 'k'
           or else (Typ = 'b' and then Is_Generic_Instance (E))

            --  Allow the generation of references to reads, writes and calls
            --  in SPARK mode when the related context comes from an instance.

           or else
             (GNATprove_Mode
               and then In_Extended_Main_Code_Unit (N)
               and then (Typ = 'm' or else Typ = 'r' or else Typ = 's'))
         then
            null;
         else
            return;
         end if;
      end if;

      --  For reference type p, the entity must be in main source unit

      if Typ = 'p' and then not In_Extended_Main_Source_Unit (E) then
         return;
      end if;

      --  Unless the reference is forced, we ignore references where the
      --  reference itself does not come from source.

      if not Force and then not Comes_From_Source (N) then
         return;
      end if;

      --  Deal with setting entity as referenced, unless suppressed. Note that
      --  we still do Set_Referenced on entities that do not come from source.
      --  This situation arises when we have a source reference to a derived
      --  operation, where the derived operation itself does not come from
      --  source, but we still want to mark it as referenced, since we really
      --  are referencing an entity in the corresponding package (this avoids
      --  wrong complaints that the package contains no referenced entities).

      if Set_Ref then

         --  Assignable object appearing on left side of assignment or as
         --  an out parameter.

         if Is_Assignable (E)
           and then Is_On_LHS (N)
           and then Ekind (E) /= E_In_Out_Parameter
         then
            --  For objects that are renamings, just set as simply referenced
            --  we do not try to do assignment type tracking in this case.

            if Present (Renamed_Object (E)) then
               Set_Referenced (E);

            --  Out parameter case

            elsif Kind = E_Out_Parameter then

               --  If warning mode for all out parameters is set, or this is
               --  the only warning parameter, then we want to mark this for
               --  later warning logic by setting Referenced_As_Out_Parameter

               if Warn_On_Modified_As_Out_Parameter (Formal) then
                  Set_Referenced_As_Out_Parameter (E, True);
                  Set_Referenced_As_LHS (E, False);

               --  For OUT parameter not covered by the above cases, we simply
               --  regard it as a normal reference (in this case we do not
               --  want any of the warning machinery for out parameters).

               else
                  Set_Referenced (E);
               end if;

            --  For the left hand of an assignment case, we do nothing here.
            --  The processing for Analyze_Assignment_Statement will set the
            --  Referenced_As_LHS flag.

            else
               null;
            end if;

         --  Check for a reference in a pragma that should not count as a
         --  making the variable referenced for warning purposes.

         elsif Is_Non_Significant_Pragma_Reference (N) then
            null;

         --  A reference in an attribute definition clause does not count as a
         --  reference except for the case of Address. The reason that 'Address
         --  is an exception is that it creates an alias through which the
         --  variable may be referenced.

         elsif Nkind (Parent (N)) = N_Attribute_Definition_Clause
           and then Chars (Parent (N)) /= Name_Address
           and then N = Name (Parent (N))
         then
            null;

         --  Constant completion does not count as a reference

         elsif Typ = 'c'
           and then Ekind (E) = E_Constant
         then
            null;

         --  Record representation clause does not count as a reference

         elsif Nkind (N) = N_Identifier
           and then Nkind (Parent (N)) = N_Record_Representation_Clause
         then
            null;

         --  Discriminants do not need to produce a reference to record type

         elsif Typ = 'd'
           and then Nkind (Parent (N)) = N_Discriminant_Specification
         then
            null;

         --  All other cases

         else
            --  Special processing for IN OUT parameters, where we have an
            --  implicit assignment to a simple variable.

            if Kind = E_In_Out_Parameter
              and then Is_Assignable (E)
            then
               --  For sure this counts as a normal read reference

               Set_Referenced (E);
               Set_Last_Assignment (E, Empty);

               --  We count it as being referenced as an out parameter if the
               --  option is set to warn on all out parameters, except that we
               --  have a special exclusion for an intrinsic subprogram, which
               --  is most likely an instantiation of Unchecked_Deallocation
               --  which we do not want to consider as an assignment since it
               --  generates false positives. We also exclude the case of an
               --  IN OUT parameter if the name of the procedure is Free,
               --  since we suspect similar semantics.

               if Warn_On_All_Unread_Out_Parameters
                 and then Is_Entity_Name (Name (Call))
                 and then not Is_Intrinsic_Subprogram (Entity (Name (Call)))
                 and then Chars (Name (Call)) /= Name_Free
               then
                  Set_Referenced_As_Out_Parameter (E, True);
                  Set_Referenced_As_LHS (E, False);
               end if;

            --  Don't count a recursive reference within a subprogram as a
            --  reference (that allows detection of a recursive subprogram
            --  whose only references are recursive calls as unreferenced).

            elsif Is_Subprogram (E)
              and then E = Nearest_Dynamic_Scope (Current_Scope)
            then
               null;

            --  Any other occurrence counts as referencing the entity

            elsif OK_To_Set_Referenced then
               Set_Referenced (E);

               --  If variable, this is an OK reference after an assignment
               --  so we can clear the Last_Assignment indication.

               if Is_Assignable (E) then
                  Set_Last_Assignment (E, Empty);
               end if;
            end if;
         end if;

         --  Check for pragma Unreferenced given and reference is within
         --  this source unit (occasion for possible warning to be issued).

         if Has_Unreferenced (E)
           and then In_Same_Extended_Unit (E, N)
         then
            --  A reference as a named parameter in a call does not count
            --  as a violation of pragma Unreferenced for this purpose...

            if Nkind (N) = N_Identifier
              and then Nkind (Parent (N)) = N_Parameter_Association
              and then Selector_Name (Parent (N)) = N
            then
               null;

            --  ... Neither does a reference to a variable on the left side
            --  of an assignment.

            elsif Is_On_LHS (N) then
               null;

            --  For entry formals, we want to place the warning message on the
            --  corresponding entity in the accept statement. The current scope
            --  is the body of the accept, so we find the formal whose name
            --  matches that of the entry formal (there is no link between the
            --  two entities, and the one in the accept statement is only used
            --  for conformance checking).

            elsif Ekind (Scope (E)) = E_Entry then
               declare
                  BE : Entity_Id;

               begin
                  BE := First_Entity (Current_Scope);
                  while Present (BE) loop
                     if Chars (BE) = Chars (E) then
                        Error_Msg_NE -- CODEFIX
                          ("??pragma Unreferenced given for&!", N, BE);
                        exit;
                     end if;

                     Next_Entity (BE);
                  end loop;
               end;

            --  Here we issue the warning, since this is a real reference

            else
               Error_Msg_NE -- CODEFIX
                 ("?pragma Unreferenced given for&!", N, E);
            end if;
         end if;

         --  If this is a subprogram instance, mark as well the internal
         --  subprogram in the wrapper package, which may be a visible
         --  compilation unit.

         if Is_Overloadable (E)
           and then Is_Generic_Instance (E)
           and then Present (Alias (E))
         then
            Set_Referenced (Alias (E));
         end if;
      end if;

      --  Generate reference if all conditions are met:

      if
         --  Cross referencing must be active

         Opt.Xref_Active

         --  The entity must be one for which we collect references

         and then Xref_Entity_Letters (Ekind (E)) /= ' '

         --  Both Sloc values must be set to something sensible

         and then Sloc (E) > No_Location
         and then Sloc (N) > No_Location

         --  Ignore references from within an instance. The only exceptions to
         --  this are default subprograms, for which we generate an implicit
         --  reference and compilations in SPARK mode.

         and then
           (Instantiation_Location (Sloc (N)) = No_Location
             or else Typ = 'i'
             or else GNATprove_Mode)

        --  Ignore dummy references

        and then Typ /= ' '
      then
         if Nkind_In (N, N_Identifier,
                         N_Defining_Identifier,
                         N_Defining_Operator_Symbol,
                         N_Operator_Symbol,
                         N_Defining_Character_Literal)
           or else Nkind (N) in N_Op
           or else (Nkind (N) = N_Character_Literal
                     and then Sloc (Entity (N)) /= Standard_Location)
         then
            Nod := N;

         elsif Nkind_In (N, N_Expanded_Name, N_Selected_Component) then
            Nod := Selector_Name (N);

         else
            return;
         end if;

         --  Normal case of source entity comes from source

         if Comes_From_Source (E) then
            Ent := E;

         --  Entity does not come from source, but is a derived subprogram and
         --  the derived subprogram comes from source (after one or more
         --  derivations) in which case the reference is to parent subprogram.

         elsif Is_Overloadable (E)
           and then Present (Alias (E))
         then
            Ent := Alias (E);
            while not Comes_From_Source (Ent) loop
               if No (Alias (Ent)) then
                  return;
               end if;

               Ent := Alias (Ent);
            end loop;

         --  The internally created defining entity for a child subprogram
         --  that has no previous spec has valid references.

         elsif Is_Overloadable (E)
           and then Is_Child_Unit (E)
         then
            Ent := E;

         --  Ditto for the formals of such a subprogram

         elsif Is_Overloadable (Scope (E))
           and then Is_Child_Unit (Scope (E))
         then
            Ent := E;

         --  Record components of discriminated subtypes or derived types must
         --  be treated as references to the original component.

         elsif Ekind (E) = E_Component
           and then Comes_From_Source (Original_Record_Component (E))
         then
            Ent := Original_Record_Component (E);

         --  If this is an expanded reference to a discriminant, recover the
         --  original discriminant, which gets the reference.

         elsif Ekind (E) = E_In_Parameter
           and then  Present (Discriminal_Link (E))
         then
            Ent := Discriminal_Link (E);
            Set_Referenced (Ent);

         --  Ignore reference to any other entity that is not from source

         else
            return;
         end if;

         --  In SPARK mode, consider the underlying entity renamed instead of
         --  the renaming, which is needed to compute a valid set of effects
         --  (reads, writes) for the enclosing subprogram.

         if GNATprove_Mode then
            Ent := Get_Through_Renamings (Ent);

            --  If no enclosing object, then it could be a reference to any
            --  location not tracked individually, like heap-allocated data.
            --  Conservatively approximate this possibility by generating a
            --  dereference, and return.

            if No (Ent) then
               if Actual_Typ = 'w' then
                  SPARK_Specific.Generate_Dereference (Nod, 'r');
                  SPARK_Specific.Generate_Dereference (Nod, 'w');
               else
                  SPARK_Specific.Generate_Dereference (Nod, 'r');
               end if;

               return;
            end if;
         end if;

         --  Record reference to entity

         if Actual_Typ = 'p'
           and then Is_Subprogram (Nod)
           and then Present (Overridden_Operation (Nod))
         then
            Actual_Typ := 'P';
         end if;

         --  Comment needed here for special SPARK code ???

         if GNATprove_Mode then
            Ref := Sloc (Nod);
            Def := Sloc (Ent);

            Ref_Scope :=
              SPARK_Specific.Enclosing_Subprogram_Or_Library_Package (Nod);
            Ent_Scope :=
              SPARK_Specific.Enclosing_Subprogram_Or_Library_Package (Ent);

            --  Since we are reaching through renamings in SPARK mode, we may
            --  end up with standard constants. Ignore those.

            if Sloc (Ent_Scope) <= Standard_Location
              or else Def <= Standard_Location
            then
               return;
            end if;

            Add_Entry
              ((Ent      => Ent,
                Loc       => Ref,
                Typ       => Actual_Typ,
                Eun       => Get_Code_Unit (Def),
                Lun       => Get_Code_Unit (Ref),
                Ref_Scope => Ref_Scope,
                Ent_Scope => Ent_Scope),
               Ent_Scope_File => Get_Code_Unit (Ent));

         else
            Ref := Original_Location (Sloc (Nod));
            Def := Original_Location (Sloc (Ent));

            --  If this is an operator symbol, skip the initial quote for
            --  navigation purposes. This is not done for the end label,
            --  where we want the actual position after the closing quote.

            if Typ = 't' then
               null;

            elsif Nkind (N) = N_Defining_Operator_Symbol
              or else Nkind (Nod) = N_Operator_Symbol
            then
               Ref := Ref + 1;
            end if;

            Add_Entry
              ((Ent       => Ent,
                Loc       => Ref,
                Typ       => Actual_Typ,
                Eun       => Get_Source_Unit (Def),
                Lun       => Get_Source_Unit (Ref),
                Ref_Scope => Empty,
                Ent_Scope => Empty),
               Ent_Scope_File => No_Unit);

            --  Generate reference to the first private entity

            if Typ = 'e'
              and then Comes_From_Source (E)
              and then Nkind (Ent) = N_Defining_Identifier
              and then (Is_Package_Or_Generic_Package (Ent)
                         or else Is_Concurrent_Type (Ent))
              and then Present (First_Private_Entity (E))
              and then In_Extended_Main_Source_Unit (N)
            then
               --  Handle case in which the full-view and partial-view of the
               --  first private entity are swapped

               declare
                  First_Private : Entity_Id := First_Private_Entity (E);

               begin
                  if Is_Private_Type (First_Private)
                    and then Present (Full_View (First_Private))
                  then
                     First_Private := Full_View (First_Private);
                  end if;

                  Add_Entry
                    ((Ent       => Ent,
                      Loc       => Sloc (First_Private),
                      Typ       => 'E',
                      Eun       => Get_Source_Unit (Def),
                      Lun       => Get_Source_Unit (Ref),
                      Ref_Scope => Empty,
                      Ent_Scope => Empty),
                     Ent_Scope_File => No_Unit);
               end;
            end if;
         end if;
      end if;
   end Generate_Reference;

   -----------------------------------
   -- Generate_Reference_To_Formals --
   -----------------------------------

   procedure Generate_Reference_To_Formals (E : Entity_Id) is
      Formal : Entity_Id;

   begin
      if Is_Generic_Subprogram (E) then
         Formal := First_Entity (E);

         while Present (Formal)
           and then not Is_Formal (Formal)
         loop
            Next_Entity (Formal);
         end loop;

      elsif Ekind (E) in Access_Subprogram_Kind then
         Formal := First_Formal (Designated_Type (E));

      else
         Formal := First_Formal (E);
      end if;

      while Present (Formal) loop
         if Ekind (Formal) = E_In_Parameter then

            if Nkind (Parameter_Type (Parent (Formal)))
              = N_Access_Definition
            then
               Generate_Reference (E, Formal, '^', False);
            else
               Generate_Reference (E, Formal, '>', False);
            end if;

         elsif Ekind (Formal) = E_In_Out_Parameter then
            Generate_Reference (E, Formal, '=', False);

         else
            Generate_Reference (E, Formal, '<', False);
         end if;

         Next_Formal (Formal);
      end loop;
   end Generate_Reference_To_Formals;

   -------------------------------------------
   -- Generate_Reference_To_Generic_Formals --
   -------------------------------------------

   procedure Generate_Reference_To_Generic_Formals (E : Entity_Id) is
      Formal : Entity_Id;

   begin
      Formal := First_Entity (E);
      while Present (Formal) loop
         if Comes_From_Source (Formal) then
            Generate_Reference (E, Formal, 'z', False);
         end if;

         Next_Entity (Formal);
      end loop;
   end Generate_Reference_To_Generic_Formals;

   -------------
   -- Get_Key --
   -------------

   function Get_Key (E : Xref_Entry_Number) return Xref_Entry_Number is
   begin
      return E;
   end Get_Key;

   ----------
   -- Hash --
   ----------

   function Hash (F : Xref_Entry_Number) return Header_Num is
      --  It is unlikely to have two references to the same entity at the same
      --  source location, so the hash function depends only on the Ent and Loc
      --  fields.

      XE : Xref_Entry renames Xrefs.Table (F);
      type M is mod 2**32;

      H : constant M := M (XE.Key.Ent) + 2 ** 7 * M (abs XE.Key.Loc);
      --  It would be more natural to write:
      --
      --    H : constant M := M'Mod (XE.Key.Ent) + 2**7 * M'Mod (XE.Key.Loc);
      --
      --  But we can't use M'Mod, because it prevents bootstrapping with older
      --  compilers. Loc can be negative, so we do "abs" before converting.
      --  One day this can be cleaned up ???

   begin
      return Header_Num (H mod Num_Buckets);
   end Hash;

   -----------------
   -- HT_Set_Next --
   -----------------

   procedure HT_Set_Next (E : Xref_Entry_Number; Next : Xref_Entry_Number) is
   begin
      Xrefs.Table (E).HTable_Next := Next;
   end HT_Set_Next;

   -------------
   -- HT_Next --
   -------------

   function HT_Next (E : Xref_Entry_Number) return Xref_Entry_Number is
   begin
      return Xrefs.Table (E).HTable_Next;
   end HT_Next;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Xrefs.Init;
   end Initialize;

   --------
   -- Lt --
   --------

   function Lt (T1, T2 : Xref_Entry) return Boolean is
   begin
      --  First test: if entity is in different unit, sort by unit

      if T1.Key.Eun /= T2.Key.Eun then
         return Dependency_Num (T1.Key.Eun) < Dependency_Num (T2.Key.Eun);

      --  Second test: within same unit, sort by entity Sloc

      elsif T1.Def /= T2.Def then
         return T1.Def < T2.Def;

      --  Third test: sort definitions ahead of references

      elsif T1.Key.Loc = No_Location then
         return True;

      elsif T2.Key.Loc = No_Location then
         return False;

      --  Fourth test: for same entity, sort by reference location unit

      elsif T1.Key.Lun /= T2.Key.Lun then
         return Dependency_Num (T1.Key.Lun) < Dependency_Num (T2.Key.Lun);

      --  Fifth test: order of location within referencing unit

      elsif T1.Key.Loc /= T2.Key.Loc then
         return T1.Key.Loc < T2.Key.Loc;

      --  Finally, for two locations at the same address, we prefer
      --  the one that does NOT have the type 'r' so that a modification
      --  or extension takes preference, when there are more than one
      --  reference at the same location. As a result, in the case of
      --  entities that are in-out actuals, the read reference follows
      --  the modify reference.

      else
         return T2.Key.Typ = 'r';
      end if;
   end Lt;

   -----------------------
   -- Output_References --
   -----------------------

   procedure Output_References is

      procedure Get_Type_Reference
        (Ent   : Entity_Id;
         Tref  : out Entity_Id;
         Left  : out Character;
         Right : out Character);
      --  Given an Entity_Id Ent, determines whether a type reference is
      --  required. If so, Tref is set to the entity for the type reference
      --  and Left and Right are set to the left/right brackets to be output
      --  for the reference. If no type reference is required, then Tref is
      --  set to Empty, and Left/Right are set to space.

      procedure Output_Import_Export_Info (Ent : Entity_Id);
      --  Output language and external name information for an interfaced
      --  entity, using the format <language, external_name>.

      ------------------------
      -- Get_Type_Reference --
      ------------------------

      procedure Get_Type_Reference
        (Ent   : Entity_Id;
         Tref  : out Entity_Id;
         Left  : out Character;
         Right : out Character)
      is
         Sav : Entity_Id;

      begin
         --  See if we have a type reference

         Tref := Ent;
         Left := '{';
         Right := '}';

         loop
            Sav := Tref;

            --  Processing for types

            if Is_Type (Tref) then

               --  Case of base type

               if Base_Type (Tref) = Tref then

                  --  If derived, then get first subtype

                  if Tref /= Etype (Tref) then
                     Tref := First_Subtype (Etype (Tref));

                     --  Set brackets for derived type, but don't override
                     --  pointer case since the fact that something is a
                     --  pointer is more important.

                     if Left /= '(' then
                        Left := '<';
                        Right := '>';
                     end if;

                  --  If the completion of a private type is itself a derived
                  --  type, we need the parent of the full view.

                  elsif Is_Private_Type (Tref)
                    and then Present (Full_View (Tref))
                    and then Etype (Full_View (Tref)) /= Full_View (Tref)
                  then
                     Tref := Etype (Full_View (Tref));

                     if Left /= '(' then
                        Left := '<';
                        Right := '>';
                     end if;

                  --  If non-derived pointer, get directly designated type.
                  --  If the type has a full view, all references are on the
                  --  partial view that is seen first.

                  elsif Is_Access_Type (Tref) then
                     Tref := Directly_Designated_Type (Tref);
                     Left := '(';
                     Right := ')';

                  elsif Is_Private_Type (Tref)
                    and then Present (Full_View (Tref))
                  then
                     if Is_Access_Type (Full_View (Tref)) then
                        Tref := Directly_Designated_Type (Full_View (Tref));
                        Left := '(';
                        Right := ')';

                     --  If the full view is an array type, we also retrieve
                     --  the corresponding component type, because the ali
                     --  entry already indicates that this is an array.

                     elsif Is_Array_Type (Full_View (Tref)) then
                        Tref := Component_Type (Full_View (Tref));
                        Left := '(';
                        Right := ')';
                     end if;

                  --  If non-derived array, get component type. Skip component
                  --  type for case of String or Wide_String, saves worthwhile
                  --  space.

                  elsif Is_Array_Type (Tref)
                    and then Tref /= Standard_String
                    and then Tref /= Standard_Wide_String
                  then
                     Tref := Component_Type (Tref);
                     Left := '(';
                     Right := ')';

                  --  For other non-derived base types, nothing

                  else
                     exit;
                  end if;

               --  For a subtype, go to ancestor subtype

               else
                  Tref := Ancestor_Subtype (Tref);

                  --  If no ancestor subtype, go to base type

                  if No (Tref) then
                     Tref := Base_Type (Sav);
                  end if;
               end if;

            --  For objects, functions, enum literals, just get type from
            --  Etype field.

            elsif Is_Object (Tref)
              or else Ekind (Tref) = E_Enumeration_Literal
              or else Ekind (Tref) = E_Function
              or else Ekind (Tref) = E_Operator
            then
               Tref := Etype (Tref);

               --  Another special case: an object of a classwide type
               --  initialized with a tag-indeterminate call gets a subtype
               --  of the classwide type during expansion. See if the original
               --  type in the declaration is named, and return it instead
               --  of going to the root type.

               if Ekind (Tref) = E_Class_Wide_Subtype
                 and then Nkind (Parent (Ent)) = N_Object_Declaration
                 and then
                   Nkind (Original_Node (Object_Definition (Parent (Ent))))
                     = N_Identifier
               then
                  Tref :=
                    Entity
                      (Original_Node ((Object_Definition (Parent (Ent)))));
               end if;

            --  For anything else, exit

            else
               exit;
            end if;

            --  Exit if no type reference, or we are stuck in some loop trying
            --  to find the type reference, or if the type is standard void
            --  type (the latter is an implementation artifact that should not
            --  show up in the generated cross-references).

            exit when No (Tref)
              or else Tref = Sav
              or else Tref = Standard_Void_Type;

            --  If we have a usable type reference, return, otherwise keep
            --  looking for something useful (we are looking for something
            --  that either comes from source or standard)

            if Sloc (Tref) = Standard_Location
              or else Comes_From_Source (Tref)
            then
               --  If the reference is a subtype created for a generic actual,
               --  go actual directly, the inner subtype is not user visible.

               if Nkind (Parent (Tref)) = N_Subtype_Declaration
                 and then not Comes_From_Source (Parent (Tref))
                 and then
                  (Is_Wrapper_Package (Scope (Tref))
                     or else Is_Generic_Instance (Scope (Tref)))
               then
                  Tref := First_Subtype (Base_Type (Tref));
               end if;

               return;
            end if;
         end loop;

         --  If we fall through the loop, no type reference

         Tref := Empty;
         Left := ' ';
         Right := ' ';
      end Get_Type_Reference;

      -------------------------------
      -- Output_Import_Export_Info --
      -------------------------------

      procedure Output_Import_Export_Info (Ent : Entity_Id) is
         Language_Name : Name_Id;
         Conv          : constant Convention_Id := Convention (Ent);

      begin
         --  Generate language name from convention

         if Conv  = Convention_C then
            Language_Name := Name_C;

         elsif Conv = Convention_CPP then
            Language_Name := Name_CPP;

         elsif Conv = Convention_Ada then
            Language_Name := Name_Ada;

         else
            --  For the moment we ignore all other cases ???

            return;
         end if;

         Write_Info_Char ('<');
         Get_Unqualified_Name_String (Language_Name);

         for J in 1 .. Name_Len loop
            Write_Info_Char (Name_Buffer (J));
         end loop;

         if Present (Interface_Name (Ent)) then
            Write_Info_Char (',');
            String_To_Name_Buffer (Strval (Interface_Name (Ent)));

            for J in 1 .. Name_Len loop
               Write_Info_Char (Name_Buffer (J));
            end loop;
         end if;

         Write_Info_Char ('>');
      end Output_Import_Export_Info;

   --  Start of processing for Output_References

   begin
      --  First we add references to the primitive operations of tagged types
      --  declared in the main unit.

      Handle_Prim_Ops : declare
         Ent  : Entity_Id;

      begin
         for J in 1 .. Xrefs.Last loop
            Ent := Xrefs.Table (J).Key.Ent;

            if Is_Type (Ent)
              and then Is_Tagged_Type (Ent)
              and then Is_Base_Type (Ent)
              and then In_Extended_Main_Source_Unit (Ent)
            then
               Generate_Prim_Op_References (Ent);
            end if;
         end loop;
      end Handle_Prim_Ops;

      --  Before we go ahead and output the references we have a problem
      --  that needs dealing with. So far we have captured things that are
      --  definitely referenced by the main unit, or defined in the main
      --  unit. That's because we don't want to clutter up the ali file
      --  for this unit with definition lines for entities in other units
      --  that are not referenced.

      --  But there is a glitch. We may reference an entity in another unit,
      --  and it may have a type reference to an entity that is not directly
      --  referenced in the main unit, which may mean that there is no xref
      --  entry for this entity yet in the list of references.

      --  If we don't do something about this, we will end with an orphan type
      --  reference, i.e. it will point to an entity that does not appear
      --  within the generated references in the ali file. That is not good for
      --  tools using the xref information.

      --  To fix this, we go through the references adding definition entries
      --  for any unreferenced entities that can be referenced in a type
      --  reference. There is a recursion problem here, and that is dealt with
      --  by making sure that this traversal also traverses any entries that
      --  get added by the traversal.

      Handle_Orphan_Type_References : declare
         J    : Nat;
         Tref : Entity_Id;
         Ent  : Entity_Id;

         L, R : Character;
         pragma Warnings (Off, L);
         pragma Warnings (Off, R);

         procedure New_Entry (E : Entity_Id);
         --  Make an additional entry into the Xref table for a type entity
         --  that is related to the current entity (parent, type ancestor,
         --  progenitor, etc.).

         ----------------
         -- New_Entry --
         ----------------

         procedure New_Entry (E : Entity_Id) is
         begin
            pragma Assert (Present (E));

            if not Has_Xref_Entry (Implementation_Base_Type (E))
              and then Sloc (E) > No_Location
            then
               Add_Entry
                 ((Ent       => E,
                   Loc       => No_Location,
                   Typ       => Character'First,
                   Eun       => Get_Source_Unit (Original_Location (Sloc (E))),
                   Lun       => No_Unit,
                   Ref_Scope => Empty,
                   Ent_Scope => Empty),
                  Ent_Scope_File => No_Unit);
            end if;
         end New_Entry;

      --  Start of processing for Handle_Orphan_Type_References

      begin
         --  Note that this is not a for loop for a very good reason. The
         --  processing of items in the table can add new items to the table,
         --  and they must be processed as well.

         J := 1;
         while J <= Xrefs.Last loop
            Ent := Xrefs.Table (J).Key.Ent;
            Get_Type_Reference (Ent, Tref, L, R);

            if Present (Tref)
              and then not Has_Xref_Entry (Tref)
              and then Sloc (Tref) > No_Location
            then
               New_Entry (Tref);

               if Is_Record_Type (Ent)
                 and then Present (Interfaces (Ent))
               then
                  --  Add an entry for each one of the given interfaces
                  --  implemented by type Ent.

                  declare
                     Elmt : Elmt_Id := First_Elmt (Interfaces (Ent));
                  begin
                     while Present (Elmt) loop
                        New_Entry (Node (Elmt));
                        Next_Elmt (Elmt);
                     end loop;
                  end;
               end if;
            end if;

            --  Collect inherited primitive operations that may be declared in
            --  another unit and have no visible reference in the current one.

            if Is_Type (Ent)
              and then Is_Tagged_Type (Ent)
              and then Is_Derived_Type (Ent)
              and then Is_Base_Type (Ent)
              and then In_Extended_Main_Source_Unit (Ent)
            then
               declare
                  Op_List : constant Elist_Id := Primitive_Operations (Ent);
                  Op      : Elmt_Id;
                  Prim    : Entity_Id;

                  function Parent_Op (E : Entity_Id) return Entity_Id;
                  --  Find original operation, which may be inherited through
                  --  several derivations.

                  function Parent_Op (E : Entity_Id) return Entity_Id is
                     Orig_Op : constant Entity_Id := Alias (E);

                  begin
                     if No (Orig_Op) then
                        return Empty;

                     elsif not Comes_From_Source (E)
                       and then not Has_Xref_Entry (Orig_Op)
                       and then Comes_From_Source (Orig_Op)
                     then
                        return Orig_Op;
                     else
                        return Parent_Op (Orig_Op);
                     end if;
                  end Parent_Op;

               begin
                  Op := First_Elmt (Op_List);
                  while Present (Op) loop
                     Prim := Parent_Op (Node (Op));

                     if Present (Prim) then
                        Add_Entry
                          ((Ent       => Prim,
                            Loc       => No_Location,
                            Typ       => Character'First,
                            Eun       => Get_Source_Unit (Sloc (Prim)),
                            Lun       => No_Unit,
                            Ref_Scope => Empty,
                            Ent_Scope => Empty),
                           Ent_Scope_File => No_Unit);
                     end if;

                     Next_Elmt (Op);
                  end loop;
               end;
            end if;

            J := J + 1;
         end loop;
      end Handle_Orphan_Type_References;

      --  Now we have all the references, including those for any embedded type
      --  references, so we can sort them, and output them.

      Output_Refs : declare

         Nrefs : constant Nat := Xrefs.Last;
         --  Number of references in table

         Rnums : array (0 .. Nrefs) of Nat;
         --  This array contains numbers of references in the Xrefs table.
         --  This list is sorted in output order. The extra 0'th entry is
         --  convenient for the call to sort. When we sort the table, we
         --  move the entries in Rnums around, but we do not move the
         --  original table entries.

         Curxu : Unit_Number_Type;
         --  Current xref unit

         Curru : Unit_Number_Type;
         --  Current reference unit for one entity

         Curent : Entity_Id;
         --  Current entity

         Curnam : String (1 .. Name_Buffer'Length);
         Curlen : Natural;
         --  Simple name and length of current entity

         Curdef : Source_Ptr;
         --  Original source location for current entity

         Crloc : Source_Ptr;
         --  Current reference location

         Ctyp : Character;
         --  Entity type character

         Prevt : Character;
         --  reference kind of previous reference

         Tref : Entity_Id;
         --  Type reference

         Rref : Node_Id;
         --  Renaming reference

         Trunit : Unit_Number_Type;
         --  Unit number for type reference

         function Lt (Op1, Op2 : Natural) return Boolean;
         --  Comparison function for Sort call

         function Name_Change (X : Entity_Id) return Boolean;
         --  Determines if entity X has a different simple name from Curent

         procedure Move (From : Natural; To : Natural);
         --  Move procedure for Sort call

         package Sorting is new GNAT.Heap_Sort_G (Move, Lt);

         --------
         -- Lt --
         --------

         function Lt (Op1, Op2 : Natural) return Boolean is
            T1 : Xref_Entry renames Xrefs.Table (Rnums (Nat (Op1)));
            T2 : Xref_Entry renames Xrefs.Table (Rnums (Nat (Op2)));

         begin
            return Lt (T1, T2);
         end Lt;

         ----------
         -- Move --
         ----------

         procedure Move (From : Natural; To : Natural) is
         begin
            Rnums (Nat (To)) := Rnums (Nat (From));
         end Move;

         -----------------
         -- Name_Change --
         -----------------

         --  Why a string comparison here??? Why not compare Name_Id values???

         function Name_Change (X : Entity_Id) return Boolean is
         begin
            Get_Unqualified_Name_String (Chars (X));

            if Name_Len /= Curlen then
               return True;
            else
               return Name_Buffer (1 .. Curlen) /= Curnam (1 .. Curlen);
            end if;
         end Name_Change;

      --  Start of processing for Output_Refs

      begin
         --  Capture the definition Sloc values. We delay doing this till now,
         --  since at the time the reference or definition is made, private
         --  types may be swapped, and the Sloc value may be incorrect. We
         --  also set up the pointer vector for the sort.

         --  For user-defined operators we need to skip the initial quote and
         --  point to the first character of the name, for navigation purposes.

         for J in 1 .. Nrefs loop
            declare
               E   : constant Entity_Id  := Xrefs.Table (J).Key.Ent;
               Loc : constant Source_Ptr := Original_Location (Sloc (E));

            begin
               Rnums (J) := J;

               if Nkind (E) = N_Defining_Operator_Symbol then
                  Xrefs.Table (J).Def := Loc + 1;
               else
                  Xrefs.Table (J).Def := Loc;
               end if;
            end;
         end loop;

         --  Sort the references

         Sorting.Sort (Integer (Nrefs));

         --  Initialize loop through references

         Curxu  := No_Unit;
         Curent := Empty;
         Curdef := No_Location;
         Curru  := No_Unit;
         Crloc  := No_Location;
         Prevt  := 'm';

         --  Loop to output references

         for Refno in 1 .. Nrefs loop
            Output_One_Ref : declare
               Ent : Entity_Id;

               XE : Xref_Entry renames Xrefs.Table (Rnums (Refno));
               --  The current entry to be accessed

               Left  : Character;
               Right : Character;
               --  Used for {} or <> or () for type reference

               procedure Check_Type_Reference
                 (Ent            : Entity_Id;
                  List_Interface : Boolean);
               --  Find whether there is a meaningful type reference for
               --  Ent, and display it accordingly. If List_Interface is
               --  true, then Ent is a progenitor interface of the current
               --  type entity being listed. In that case list it as is,
               --  without looking for a type reference for it.

               procedure Output_Instantiation_Refs (Loc : Source_Ptr);
               --  Recursive procedure to output instantiation references for
               --  the given source ptr in [file|line[...]] form. No output
               --  if the given location is not a generic template reference.

               procedure Output_Overridden_Op (Old_E : Entity_Id);
               --  For a subprogram that is overriding, display information
               --  about the inherited operation that it overrides.

               --------------------------
               -- Check_Type_Reference --
               --------------------------

               procedure Check_Type_Reference
                 (Ent            : Entity_Id;
                  List_Interface : Boolean)
               is
               begin
                  if List_Interface then

                     --  This is a progenitor interface of the type for which
                     --  xref information is being generated.

                     Tref  := Ent;
                     Left  := '<';
                     Right := '>';

                  else
                     Get_Type_Reference (Ent, Tref, Left, Right);
                  end if;

                  if Present (Tref) then

                     --  Case of standard entity, output name

                     if Sloc (Tref) = Standard_Location then
                        Write_Info_Char (Left);
                        Write_Info_Name (Chars (Tref));
                        Write_Info_Char (Right);

                     --  Case of source entity, output location

                     else
                        Write_Info_Char (Left);
                        Trunit := Get_Source_Unit (Sloc (Tref));

                        if Trunit /= Curxu then
                           Write_Info_Nat (Dependency_Num (Trunit));
                           Write_Info_Char ('|');
                        end if;

                        Write_Info_Nat
                          (Int (Get_Logical_Line_Number (Sloc (Tref))));

                        declare
                           Ent  : Entity_Id;
                           Ctyp : Character;

                        begin
                           Ent := Tref;
                           Ctyp := Xref_Entity_Letters (Ekind (Ent));

                           if Ctyp = '+'
                             and then Present (Full_View (Ent))
                           then
                              Ent := Underlying_Type (Ent);

                              if Present (Ent) then
                                 Ctyp := Xref_Entity_Letters (Ekind (Ent));
                              end if;
                           end if;

                           Write_Info_Char (Ctyp);
                        end;

                        Write_Info_Nat
                          (Int (Get_Column_Number (Sloc (Tref))));

                        --  If the type comes from an instantiation, add the
                        --  corresponding info.

                        Output_Instantiation_Refs (Sloc (Tref));
                        Write_Info_Char (Right);
                     end if;
                  end if;
               end Check_Type_Reference;

               -------------------------------
               -- Output_Instantiation_Refs --
               -------------------------------

               procedure Output_Instantiation_Refs (Loc : Source_Ptr) is
                  Iloc : constant Source_Ptr := Instantiation_Location (Loc);
                  Lun  : Unit_Number_Type;
                  Cu   : constant Unit_Number_Type := Curru;

               begin
                  --  Nothing to do if this is not an instantiation

                  if Iloc = No_Location then
                     return;
                  end if;

                  --  Output instantiation reference

                  Write_Info_Char ('[');
                  Lun := Get_Source_Unit (Iloc);

                  if Lun /= Curru then
                     Curru := Lun;
                     Write_Info_Nat (Dependency_Num (Curru));
                     Write_Info_Char ('|');
                  end if;

                  Write_Info_Nat (Int (Get_Logical_Line_Number (Iloc)));

                  --  Recursive call to get nested instantiations

                  Output_Instantiation_Refs (Iloc);

                  --  Output final ] after call to get proper nesting

                  Write_Info_Char (']');
                  Curru := Cu;
                  return;
               end Output_Instantiation_Refs;

               --------------------------
               -- Output_Overridden_Op --
               --------------------------

               procedure Output_Overridden_Op (Old_E : Entity_Id) is
                  Op : Entity_Id;

               begin
                  --  The overridden operation has an implicit declaration
                  --  at the point of derivation. What we want to display
                  --  is the original operation, which has the actual body
                  --  (or abstract declaration) that is being overridden.
                  --  The overridden operation is not always set, e.g. when
                  --  it is a predefined operator.

                  if No (Old_E) then
                     return;

                  --  Follow alias chain if one is present

                  elsif Present (Alias (Old_E)) then

                     --  The subprogram may have been implicitly inherited
                     --  through several levels of derivation, so find the
                     --  ultimate (source) ancestor.

                     Op := Ultimate_Alias (Old_E);

                  --  Normal case of no alias present. We omit generated
                  --  primitives like tagged equality, that have no source
                  --  representation.

                  else
                     Op := Old_E;
                  end if;

                  if Present (Op)
                    and then Sloc (Op) /= Standard_Location
                    and then Comes_From_Source (Op)
                  then
                     declare
                        Loc      : constant Source_Ptr := Sloc (Op);
                        Par_Unit : constant Unit_Number_Type :=
                                     Get_Source_Unit (Loc);

                     begin
                        Write_Info_Char ('<');

                        if Par_Unit /= Curxu then
                           Write_Info_Nat (Dependency_Num (Par_Unit));
                           Write_Info_Char ('|');
                        end if;

                        Write_Info_Nat (Int (Get_Logical_Line_Number (Loc)));
                        Write_Info_Char ('p');
                        Write_Info_Nat (Int (Get_Column_Number (Loc)));
                        Write_Info_Char ('>');
                     end;
                  end if;
               end Output_Overridden_Op;

            --  Start of processing for Output_One_Ref

            begin
               Ent := XE.Key.Ent;
               Ctyp := Xref_Entity_Letters (Ekind (Ent));

               --  Skip reference if it is the only reference to an entity,
               --  and it is an END line reference, and the entity is not in
               --  the current extended source. This prevents junk entries
               --  consisting only of packages with END lines, where no
               --  entity from the package is actually referenced.

               if XE.Key.Typ = 'e'
                 and then Ent /= Curent
                 and then (Refno = Nrefs
                            or else
                              Ent /= Xrefs.Table (Rnums (Refno + 1)).Key.Ent)
                 and then not In_Extended_Main_Source_Unit (Ent)
               then
                  goto Continue;
               end if;

               --  For private type, get full view type

               if Ctyp = '+'
                 and then Present (Full_View (XE.Key.Ent))
               then
                  Ent := Underlying_Type (Ent);

                  if Present (Ent) then
                     Ctyp := Xref_Entity_Letters (Ekind (Ent));
                  end if;
               end if;

               --  Special exception for Boolean

               if Ctyp = 'E' and then Is_Boolean_Type (Ent) then
                  Ctyp := 'B';
               end if;

               --  For variable reference, get corresponding type

               if Ctyp = '*' then
                  Ent := Etype (XE.Key.Ent);
                  Ctyp := Fold_Lower (Xref_Entity_Letters (Ekind (Ent)));

                  --  If variable is private type, get full view type

                  if Ctyp = '+'
                    and then Present (Full_View (Etype (XE.Key.Ent)))
                  then
                     Ent := Underlying_Type (Etype (XE.Key.Ent));

                     if Present (Ent) then
                        Ctyp := Fold_Lower (Xref_Entity_Letters (Ekind (Ent)));
                     end if;

                  elsif Is_Generic_Type (Ent) then

                     --  If the type of the entity is a generic private type,
                     --  there is no usable full view, so retain the indication
                     --  that this is an object.

                     Ctyp := '*';
                  end if;

                  --  Special handling for access parameters and objects and
                  --  components of an anonymous access type.

                  if Ekind_In (Etype (XE.Key.Ent),
                               E_Anonymous_Access_Type,
                               E_Anonymous_Access_Subprogram_Type,
                               E_Anonymous_Access_Protected_Subprogram_Type)
                  then
                     if Is_Formal (XE.Key.Ent)
                       or else
                         Ekind_In
                           (XE.Key.Ent, E_Variable, E_Constant, E_Component)
                     then
                        Ctyp := 'p';
                     end if;

                     --  Special handling for Boolean

                  elsif Ctyp = 'e' and then Is_Boolean_Type (Ent) then
                     Ctyp := 'b';
                  end if;
               end if;

               --  Special handling for abstract types and operations

               if Is_Overloadable (XE.Key.Ent)
                 and then Is_Abstract_Subprogram (XE.Key.Ent)
               then
                  if Ctyp = 'U' then
                     Ctyp := 'x';            --  Abstract procedure

                  elsif Ctyp = 'V' then
                     Ctyp := 'y';            --  Abstract function
                  end if;

               elsif Is_Type (XE.Key.Ent)
                 and then Is_Abstract_Type (XE.Key.Ent)
               then
                  if Is_Interface (XE.Key.Ent) then
                     Ctyp := 'h';

                  elsif Ctyp = 'R' then
                     Ctyp := 'H';            --  Abstract type
                  end if;
               end if;

               --  Only output reference if interesting type of entity

               if Ctyp = ' '

               --  Suppress references to object definitions, used for local
               --  references.

                 or else XE.Key.Typ = 'D'
                 or else XE.Key.Typ = 'I'

               --  Suppress self references, except for bodies that act as
               --  specs.

                 or else (XE.Key.Loc = XE.Def
                           and then
                             (XE.Key.Typ /= 'b'
                               or else not Is_Subprogram (XE.Key.Ent)))

               --  Also suppress definitions of body formals (we only
               --  treat these as references, and the references were
               --  separately recorded).

                 or else (Is_Formal (XE.Key.Ent)
                           and then Present (Spec_Entity (XE.Key.Ent)))
               then
                  null;

               else
                  --  Start new Xref section if new xref unit

                  if XE.Key.Eun /= Curxu then
                     if Write_Info_Col > 1 then
                        Write_Info_EOL;
                     end if;

                     Curxu := XE.Key.Eun;

                     Write_Info_Initiate ('X');
                     Write_Info_Char (' ');
                     Write_Info_Nat (Dependency_Num (XE.Key.Eun));
                     Write_Info_Char (' ');
                     Write_Info_Name
                       (Reference_Name (Source_Index (XE.Key.Eun)));
                  end if;

                  --  Start new Entity line if new entity. Note that we
                  --  consider two entities the same if they have the same
                  --  name and source location. This causes entities in
                  --  instantiations to be treated as though they referred
                  --  to the template.

                  if No (Curent)
                    or else
                      (XE.Key.Ent /= Curent
                         and then
                           (Name_Change (XE.Key.Ent) or else XE.Def /= Curdef))
                  then
                     Curent := XE.Key.Ent;
                     Curdef := XE.Def;

                     Get_Unqualified_Name_String (Chars (XE.Key.Ent));
                     Curlen := Name_Len;
                     Curnam (1 .. Curlen) := Name_Buffer (1 .. Curlen);

                     if Write_Info_Col > 1 then
                        Write_Info_EOL;
                     end if;

                     --  Write column number information

                     Write_Info_Nat (Int (Get_Logical_Line_Number (XE.Def)));
                     Write_Info_Char (Ctyp);
                     Write_Info_Nat (Int (Get_Column_Number (XE.Def)));

                     --  Write level information

                     Write_Level_Info : declare
                        function Is_Visible_Generic_Entity
                          (E : Entity_Id) return Boolean;
                        --  Check whether E is declared in the visible part
                        --  of a generic package. For source navigation
                        --  purposes, treat this as a visible entity.

                        function Is_Private_Record_Component
                          (E : Entity_Id) return Boolean;
                        --  Check whether E is a non-inherited component of a
                        --  private extension. Even if the enclosing record is
                        --  public, we want to treat the component as private
                        --  for navigation purposes.

                        ---------------------------------
                        -- Is_Private_Record_Component --
                        ---------------------------------

                        function Is_Private_Record_Component
                          (E : Entity_Id) return Boolean
                        is
                           S : constant Entity_Id := Scope (E);
                        begin
                           return
                             Ekind (E) = E_Component
                               and then Nkind (Declaration_Node (S)) =
                                 N_Private_Extension_Declaration
                               and then Original_Record_Component (E) = E;
                        end Is_Private_Record_Component;

                        -------------------------------
                        -- Is_Visible_Generic_Entity --
                        -------------------------------

                        function Is_Visible_Generic_Entity
                          (E : Entity_Id) return Boolean
                        is
                           Par : Node_Id;

                        begin
                           --  The Present check here is an error defense

                           if Present (Scope (E))
                             and then Ekind (Scope (E)) /= E_Generic_Package
                           then
                              return False;
                           end if;

                           Par := Parent (E);
                           while Present (Par) loop
                              if
                                Nkind (Par) = N_Generic_Package_Declaration
                              then
                                 --  Entity is a generic formal

                                 return False;

                              elsif
                                Nkind (Parent (Par)) = N_Package_Specification
                              then
                                 return
                                   Is_List_Member (Par)
                                     and then List_Containing (Par) =
                                       Visible_Declarations (Parent (Par));
                              else
                                 Par := Parent (Par);
                              end if;
                           end loop;

                           return False;
                        end Is_Visible_Generic_Entity;

                     --  Start of processing for Write_Level_Info

                     begin
                        if Is_Hidden (Curent)
                          or else Is_Private_Record_Component (Curent)
                        then
                           Write_Info_Char (' ');

                        elsif
                           Is_Public (Curent)
                             or else Is_Visible_Generic_Entity (Curent)
                        then
                           Write_Info_Char ('*');

                        else
                           Write_Info_Char (' ');
                        end if;
                     end Write_Level_Info;

                     --  Output entity name. We use the occurrence from the
                     --  actual source program at the definition point.

                     declare
                        Ent_Name : constant String :=
                                     Exact_Source_Name (Sloc (XE.Key.Ent));
                     begin
                        for C in Ent_Name'Range loop
                           Write_Info_Char (Ent_Name (C));
                        end loop;
                     end;

                     --  See if we have a renaming reference

                     if Is_Object (XE.Key.Ent)
                       and then Present (Renamed_Object (XE.Key.Ent))
                     then
                        Rref := Renamed_Object (XE.Key.Ent);

                     elsif Is_Overloadable (XE.Key.Ent)
                       and then Nkind (Parent (Declaration_Node (XE.Key.Ent)))
                                           = N_Subprogram_Renaming_Declaration
                     then
                        Rref := Name (Parent (Declaration_Node (XE.Key.Ent)));

                     elsif Ekind (XE.Key.Ent) = E_Package
                       and then Nkind (Declaration_Node (XE.Key.Ent)) =
                                         N_Package_Renaming_Declaration
                     then
                        Rref := Name (Declaration_Node (XE.Key.Ent));

                     else
                        Rref := Empty;
                     end if;

                     if Present (Rref) then
                        if Nkind (Rref) = N_Expanded_Name then
                           Rref := Selector_Name (Rref);
                        end if;

                        if Nkind (Rref) = N_Identifier
                          or else Nkind (Rref) = N_Operator_Symbol
                        then
                           null;

                        --  For renamed array components, use the array name
                        --  for the renamed entity, which reflect the fact that
                        --  in general the whole array is aliased.

                        elsif Nkind (Rref) = N_Indexed_Component then
                           if Nkind (Prefix (Rref)) = N_Identifier then
                              Rref := Prefix (Rref);
                           elsif Nkind (Prefix (Rref)) = N_Expanded_Name then
                              Rref := Selector_Name (Prefix (Rref));
                           else
                              Rref := Empty;
                           end if;

                        else
                           Rref := Empty;
                        end if;
                     end if;

                     --  Write out renaming reference if we have one

                     if Present (Rref) then
                        Write_Info_Char ('=');
                        Write_Info_Nat
                          (Int (Get_Logical_Line_Number (Sloc (Rref))));
                        Write_Info_Char (':');
                        Write_Info_Nat
                          (Int (Get_Column_Number (Sloc (Rref))));
                     end if;

                     --  Indicate that the entity is in the unit of the current
                     --  xref section.

                     Curru := Curxu;

                     --  Write out information about generic parent, if entity
                     --  is an instance.

                     if  Is_Generic_Instance (XE.Key.Ent) then
                        declare
                           Gen_Par : constant Entity_Id :=
                                       Generic_Parent
                                         (Specification
                                            (Unit_Declaration_Node
                                               (XE.Key.Ent)));
                           Loc     : constant Source_Ptr := Sloc (Gen_Par);
                           Gen_U   : constant Unit_Number_Type :=
                                       Get_Source_Unit (Loc);

                        begin
                           Write_Info_Char ('[');

                           if Curru /= Gen_U then
                              Write_Info_Nat (Dependency_Num (Gen_U));
                              Write_Info_Char ('|');
                           end if;

                           Write_Info_Nat
                             (Int (Get_Logical_Line_Number (Loc)));
                           Write_Info_Char (']');
                        end;
                     end if;

                     --  See if we have a type reference and if so output

                     Check_Type_Reference (XE.Key.Ent, False);

                     --  Additional information for types with progenitors,
                     --  including synchronized tagged types.

                     declare
                        Typ  : constant Entity_Id := XE.Key.Ent;
                        Elmt : Elmt_Id;

                     begin
                        if Is_Record_Type (Typ)
                          and then Present (Interfaces (Typ))
                        then
                           Elmt := First_Elmt (Interfaces (Typ));

                        elsif Is_Concurrent_Type (Typ)
                          and then Present (Corresponding_Record_Type (Typ))
                          and then Present (
                            Interfaces (Corresponding_Record_Type (Typ)))
                        then
                           Elmt :=
                             First_Elmt (
                              Interfaces (Corresponding_Record_Type (Typ)));

                        else
                           Elmt := No_Elmt;
                        end if;

                        while Present (Elmt) loop
                           Check_Type_Reference (Node (Elmt), True);
                           Next_Elmt (Elmt);
                        end loop;
                     end;

                     --  For array types, list index types as well. (This is
                     --  not C, indexes have distinct types).

                     if Is_Array_Type (XE.Key.Ent) then
                        declare
                           Indx : Node_Id;
                        begin
                           Indx := First_Index (XE.Key.Ent);
                           while Present (Indx) loop
                              Check_Type_Reference
                                (First_Subtype (Etype (Indx)), True);
                              Next_Index (Indx);
                           end loop;
                        end;
                     end if;

                     --  If the entity is an overriding operation, write info
                     --  on operation that was overridden.

                     if Is_Subprogram (XE.Key.Ent)
                       and then Present (Overridden_Operation (XE.Key.Ent))
                     then
                        Output_Overridden_Op
                          (Overridden_Operation (XE.Key.Ent));
                     end if;

                     --  End of processing for entity output

                     Crloc := No_Location;
                  end if;

                  --  Output the reference if it is not as the same location
                  --  as the previous one, or it is a read-reference that
                  --  indicates that the entity is an in-out actual in a call.

                  if XE.Key.Loc /= No_Location
                    and then
                      (XE.Key.Loc /= Crloc
                        or else (Prevt = 'm' and then  XE.Key.Typ = 'r'))
                  then
                     Crloc := XE.Key.Loc;
                     Prevt := XE.Key.Typ;

                     --  Start continuation if line full, else blank

                     if Write_Info_Col > 72 then
                        Write_Info_EOL;
                        Write_Info_Initiate ('.');
                     end if;

                     Write_Info_Char (' ');

                     --  Output file number if changed

                     if XE.Key.Lun /= Curru then
                        Curru := XE.Key.Lun;
                        Write_Info_Nat (Dependency_Num (Curru));
                        Write_Info_Char ('|');
                     end if;

                     Write_Info_Nat
                       (Int (Get_Logical_Line_Number (XE.Key.Loc)));
                     Write_Info_Char (XE.Key.Typ);

                     if Is_Overloadable (XE.Key.Ent) then
                        if (Is_Imported (XE.Key.Ent) and then XE.Key.Typ = 'b')
                             or else
                           (Is_Exported (XE.Key.Ent) and then XE.Key.Typ = 'i')
                        then
                           Output_Import_Export_Info (XE.Key.Ent);
                        end if;
                     end if;

                     Write_Info_Nat (Int (Get_Column_Number (XE.Key.Loc)));

                     Output_Instantiation_Refs (Sloc (XE.Key.Ent));
                  end if;
               end if;
            end Output_One_Ref;

         <<Continue>>
            null;
         end loop;

         Write_Info_EOL;
      end Output_Refs;
   end Output_References;

   ---------------------------------
   -- Process_Deferred_References --
   ---------------------------------

   procedure Process_Deferred_References is
   begin
      for J in Deferred_References.First .. Deferred_References.Last loop
         declare
            D : Deferred_Reference_Entry renames Deferred_References.Table (J);

         begin
            case Is_LHS (D.N) is
               when Yes =>
                  Generate_Reference (D.E, D.N, 'm');

               when No =>
                  Generate_Reference (D.E, D.N, 'r');

               --  Not clear if Unknown can occur at this stage, but if it
               --  does we will treat it as a normal reference.

               when Unknown =>
                  Generate_Reference (D.E, D.N, 'r');
            end case;
         end;
      end loop;

      --  Clear processed entries from table

      Deferred_References.Init;
   end Process_Deferred_References;

--  Start of elaboration for Lib.Xref

begin
   --  Reset is necessary because Elmt_Ptr does not default to Null_Ptr,
   --  because it's not an access type.

   Xref_Set.Reset;
end Lib.Xref;
