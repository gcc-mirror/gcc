------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                        M U T A B L Y _ T A G G E D                       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2024-2025, Free Software Foundation, Inc.         --
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

with Atree;          use Atree;
with Casing;         use Casing;
with Einfo;          use Einfo;
with Einfo.Entities; use Einfo.Entities;
with Einfo.Utils;    use Einfo.Utils;
with Exp_Util;       use Exp_Util;
with Namet;          use Namet;
with Nlists;         use Nlists;
with Nmake;          use Nmake;
with Rtsfind;        use Rtsfind;
with Snames;         use Snames;
with Sem_Util;       use Sem_Util;
with Sinfo;          use Sinfo;
with Sinfo.Nodes;    use Sinfo.Nodes;
with Sinfo.Utils;    use Sinfo.Utils;
with Stringt;        use Stringt;
with Tbuild;         use Tbuild;

package body Mutably_Tagged is

   ---------------------------------------
   -- Corresponding_Mutably_Tagged_Type --
   ---------------------------------------

   function Corresponding_Mutably_Tagged_Type
     (CW_Equiv_Typ : Entity_Id) return Entity_Id
   is
   begin
      return Class_Wide_Type (Parent_Subtype (CW_Equiv_Typ));
   end Corresponding_Mutably_Tagged_Type;

   ----------------------------------------
   -- Depends_On_Mutably_Tagged_Ext_Comp --
   ----------------------------------------

   function Depends_On_Mutably_Tagged_Ext_Comp (N : Node_Id) return Boolean is
      Typ      : Entity_Id;
      Typ_Comp : Entity_Id;
      Curr     : Node_Id;
      Prev     : Node_Id;
   begin
      --  Move through each prefix until we hit a type conversion from a
      --  mutably tagged type then check if the referenced component exists in
      --  the root type or an extension.

      Curr := N;
      while Has_Prefix (Curr) loop
         Prev := Curr;
         Curr := Prefix (Curr);

         --  Find a prefix which is a type conversion from a mutably tagged
         --  type in some form - either class-wide equivalent type or
         --  directly a mutably tagged type.

         if Nkind (Curr) in N_Unchecked_Type_Conversion
                          | N_Type_Conversion
           and then (Is_Mutably_Tagged_CW_Equivalent_Type
                       (Etype (Expression (Curr)))
                      or else Is_Mutably_Tagged_Type
                        (Etype (Expression (Curr))))

           --  Verify that the prefix references a component

           and then Is_Entity_Name (Selector_Name (Prev))
           and then Ekind (Entity (Selector_Name (Prev)))
                      = E_Component
         then
            --  Obtain the root type

            Typ := Etype (if Is_Mutably_Tagged_Type
                               (Etype (Expression (Curr)))
                          then
                             Etype (Expression (Curr))
                          else
                             Corresponding_Mutably_Tagged_Type
                               (Etype (Expression (Curr))));

            --  Move through the components of the root type looking for a
            --  match to the reference component.

            Typ_Comp := First_Component (Typ);
            while Present (Typ_Comp) loop

               --  When there is a match we know the component reference
               --  doesn't depend on a type extension.

               if Chars (Typ_Comp) = Chars (Entity (Selector_Name (Prev))) then
                  return False;
               end if;

               Next_Component (Typ_Comp);
            end loop;

            --  Otherwise, the component must depend on an extension

            return True;
         end if;
      end loop;

      --  If we get here then we know we don't have any sort of relevant type
      --  conversion from a mutably tagged object.

      return False;
   end Depends_On_Mutably_Tagged_Ext_Comp;

   ------------------------------------------------------
   -- Get_Corresponding_Mutably_Tagged_Type_If_Present --
   ------------------------------------------------------

   function Get_Corresponding_Mutably_Tagged_Type_If_Present
     (Typ : Entity_Id) return Entity_Id
   is
   begin
      if Is_Mutably_Tagged_CW_Equivalent_Type (Typ) then
         return Corresponding_Mutably_Tagged_Type (Typ);
      end if;

      return Typ;
   end Get_Corresponding_Mutably_Tagged_Type_If_Present;

   ----------------------------------------------
   -- Get_Corresponding_Tagged_Type_If_Present --
   ----------------------------------------------

   function Get_Corresponding_Tagged_Type_If_Present
     (Typ : Entity_Id) return Entity_Id
   is
   begin
      --  Obtain the related tagged type for the class-wide mutably
      --  tagged type associated with the class-wide equivalent type.

      if Is_Mutably_Tagged_CW_Equivalent_Type (Typ) then
         return Parent_Subtype (Typ);
      end if;

      return Typ;
   end Get_Corresponding_Tagged_Type_If_Present;

   ----------------------------------
   -- Is_Mutably_Tagged_Conversion --
   ----------------------------------

   function Is_Mutably_Tagged_Conversion (N : Node_Id) return Boolean is
   begin
      return Nkind (N) = N_Unchecked_Type_Conversion
               and then Is_Mutably_Tagged_CW_Equivalent_Type
                          (Etype (Expression (N)));
   end Is_Mutably_Tagged_Conversion;

   ------------------------------------------
   -- Is_Mutably_Tagged_CW_Equivalent_Type --
   ------------------------------------------

   function Is_Mutably_Tagged_CW_Equivalent_Type
     (Typ : Entity_Id) return Boolean
   is
   begin
      --  First assure Typ is OK to test since this function can be called in
      --  a context where analysis failed.

      return Present (Typ)
        and then not Error_Posted (Typ)

        --  Finally check Typ is a class-wide equivalent type which has an
        --  associated mutably tagged class-wide type (e.g. it is a class-wide
        --  type with a size clause).

        and then Is_Class_Wide_Equivalent_Type (Typ)
        and then Present (Parent_Subtype (Typ))
        and then Present (Class_Wide_Type (Parent_Subtype (Typ)))
        and then Has_Size_Clause (Corresponding_Mutably_Tagged_Type (Typ));
   end Is_Mutably_Tagged_CW_Equivalent_Type;

   --------------------------------
   -- Make_CW_Size_Compile_Check --
   --------------------------------

   function Make_CW_Size_Compile_Check
     (New_Typ     : Entity_Id;
      Mut_Tag_Typ : Entity_Id) return Node_Id
   is
      Loc : constant Source_Ptr := Sloc (New_Typ);
   begin
      --  Generate a string literal for New_Typ's name which is needed for
      --  printing within the Compile_Time_Error.

      Get_Decoded_Name_String (Chars (New_Typ));
      Set_Casing (Mixed_Case);

      --  Build a pragma Compile_Time_Error to force the backend to
      --  preform appropriate sizing checks.

      --  Generate:
      --    pragma Compile_Time_Error
      --             (New_Typ'Size < Mut_Tag_Typ'Size,
      --              "class size for by-reference type ""New_Typ"" too small")

      return
        Make_Pragma (Loc,
          Chars                        => Name_Compile_Time_Error,
          Pragma_Argument_Associations => New_List (
            Make_Pragma_Argument_Association (Loc,
              Expression => (
                Make_Op_Gt (Loc,
                  Left_Opnd  =>
                    Make_Attribute_Reference (Loc,
                      Attribute_Name => Name_Size,
                      Prefix         =>
                        New_Occurrence_Of (New_Typ, Loc)),
                  Right_Opnd =>
                    Make_Integer_Literal (Loc,
                      RM_Size (Mut_Tag_Typ))))),
             Make_Pragma_Argument_Association (Loc,
               Expression =>

                 --  Is it possible to print the size of New_Typ via
                 --  Validate_Compile_Time_Warning_Or_Error after the back-end
                 --  has run to generate the error message manually ???

                 Make_String_Literal (Loc,
                   "class size for by-reference type """
                   & To_String (String_From_Name_Buffer)
                   & """ too small"))));
   end Make_CW_Size_Compile_Check;

   ------------------------------------
   -- Make_Mutably_Tagged_Conversion --
   ------------------------------------

   procedure Make_Mutably_Tagged_Conversion
     (N     : Node_Id;
      Typ   : Entity_Id := Empty;
      Force : Boolean   := False)
   is
      Conv_Typ : constant Entity_Id :=

        --  When Typ is not present, we obtain it at this point

        (if Present (Typ) then
            Typ
         else
            Corresponding_Mutably_Tagged_Type (Etype (N)));

   begin
      --  Allow "forcing" the rewrite to an unchecked conversion

      if Force

        --  Otherwise, don't make the conversion when N is on the left-hand
        --  side of the assignment, in cases where we need the actual type
        --  such as a subtype or object renaming declaration, or a generic or
        --  parameter specification.

        --  Additionally, prevent generation of the conversion if N is already
        --  part of an unchecked conversion or a part of a selected component.

        or else (not Known_To_Be_Assigned (N, Only_LHS => True)
                  and then (No (Parent (N))
                             or else Nkind (Parent (N))
                               not in N_Selected_Component
                                    | N_Subtype_Declaration
                                    | N_Parameter_Specification
                                    | N_Generic_Association
                                    | N_Unchecked_Type_Conversion
                                    | N_Object_Renaming_Declaration))
      then
         --  Exclude the case where we have a 'Size so that we get the proper
         --  size of the class-wide equivalent type. Are there other cases ???

         if Present (Parent (N))
           and then Nkind (Parent (N)) = N_Attribute_Reference
           and then Attribute_Name (Parent (N)) in Name_Size
         then
            return;
         end if;

         --  Create the conversion

         Rewrite (N,
           Unchecked_Convert_To
             (Conv_Typ, Relocate_Node (N)));
      end if;
   end Make_Mutably_Tagged_Conversion;

   ----------------------------------
   -- Make_Mutably_Tagged_CW_Check --
   ----------------------------------

   function Make_Mutably_Tagged_CW_Check
     (N   : Node_Id;
      Tag : Node_Id) return Node_Id
   is
      Loc   : constant Source_Ptr := Sloc (N);

      --  Displace the pointer to the base of the objects applying 'Address,
      --  which is later expanded into a call to RE_Base_Address.

      N_Tag : constant Node_Id    :=
        Make_Explicit_Dereference (Loc,
          Prefix =>
            Unchecked_Convert_To (RTE (RE_Tag_Ptr),
              Make_Attribute_Reference (Loc,
                Prefix         => Duplicate_Subexpr (N),
                Attribute_Name => Name_Address)));
   begin
      --  Generate the runtime call to test class-wide membership

      return
        Make_Raise_Constraint_Error (Loc,
          Reason    => CE_Tag_Check_Failed,
          Condition =>
            Make_Op_Not (Loc,
              Make_Function_Call (Loc,
                Parameter_Associations => New_List (N_Tag, Tag),
                Name                   =>
                  New_Occurrence_Of (RTE (RE_CW_Membership), Loc))));
   end Make_Mutably_Tagged_CW_Check;

end Mutably_Tagged;
