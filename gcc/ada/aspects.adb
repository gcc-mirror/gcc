------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              A S P E C T S                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2010-2024, Free Software Foundation, Inc.         --
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
with Einfo;          use Einfo;
with Einfo.Entities; use Einfo.Entities;
with Einfo.Utils;    use Einfo.Utils;
with Nlists;         use Nlists;
with Sinfo;          use Sinfo;
with Sinfo.Nodes;    use Sinfo.Nodes;
with Sinfo.Utils;    use Sinfo.Utils;

with GNAT.HTable;

package body Aspects is

   --  The following array indicates aspects that a subtype inherits from its
   --  base type. True means that the subtype inherits the aspect from its base
   --  type. False means it is not inherited.

   Base_Aspect : constant array (Aspect_Id) of Boolean :=
     (Aspect_Atomic                 => True,
      Aspect_Atomic_Components      => True,
      Aspect_Constant_Indexing      => True,
      Aspect_Default_Iterator       => True,
      Aspect_Discard_Names          => True,
      Aspect_Independent_Components => True,
      Aspect_Iterator_Element       => True,
      Aspect_Stable_Properties      => True,
      Aspect_Type_Invariant         => True,
      Aspect_Unchecked_Union        => True,
      Aspect_Variable_Indexing      => True,
      Aspect_Volatile               => True,
      Aspect_Volatile_Full_Access   => True,
      others                        => False);

   --  The following array indicates type aspects that are inherited and apply
   --  to the class-wide type as well.

   Inherited_Aspect : constant array (Aspect_Id) of Boolean :=
     (Aspect_Constant_Indexing    => True,
      Aspect_Default_Iterator     => True,
      Aspect_Implicit_Dereference => True,
      Aspect_Iterator_Element     => True,
      Aspect_Remote_Types         => True,
      Aspect_Variable_Indexing    => True,
      others                      => False);

   -------------------------------------
   -- Hash Table for Aspect Id Values --
   -------------------------------------

   type AI_Hash_Range is range 0 .. 112;
   --  Size of hash table headers

   function AI_Hash (F : Name_Id) return AI_Hash_Range;
   --  Hash function for hash table

   function AI_Hash (F : Name_Id) return AI_Hash_Range is
   begin
      return AI_Hash_Range (F mod 113);
   end AI_Hash;

   package Aspect_Id_Hash_Table is new
     GNAT.HTable.Simple_HTable
       (Header_Num => AI_Hash_Range,
        Element    => Aspect_Id,
        No_Element => No_Aspect,
        Key        => Name_Id,
        Hash       => AI_Hash,
        Equal      => "=");

   --------------------------------
   -- Aspects_On_Body_Or_Stub_OK --
   --------------------------------

   function Aspects_On_Body_Or_Stub_OK (N : Node_Id) return Boolean is
      Aspect  : Node_Id;
      Aspects : List_Id;

   begin
      --  The routine should be invoked on a body [stub] with aspects

      pragma Assert (Has_Aspects (N));
      pragma Assert
        (Nkind (N) in N_Body_Stub      | N_Entry_Body      | N_Package_Body |
                      N_Protected_Body | N_Subprogram_Body | N_Task_Body);

      --  Look through all aspects and see whether they can be applied to a
      --  body [stub].

      Aspects := Aspect_Specifications (N);
      Aspect  := First (Aspects);
      while Present (Aspect) loop
         if not Aspect_On_Body_Or_Stub_OK (Get_Aspect_Id (Aspect)) then
            return False;
         end if;

         Next (Aspect);
      end loop;

      return True;
   end Aspects_On_Body_Or_Stub_OK;

   -----------------
   -- Find_Aspect --
   -----------------

   function Find_Aspect
     (Id            : Entity_Id;
      A             : Aspect_Id;
      Class_Present : Boolean := False;
      Or_Rep_Item   : Boolean := False) return Node_Id
   is
      Decl                 : Node_Id;
      Item                 : Node_Id;
      Owner                : Entity_Id;
      Spec                 : Node_Id;
      Alternative_Rep_Item : Node_Id := Empty;
   begin
      Owner := Id;

      --  Handle various cases of base or inherited aspects for types

      if Is_Type (Id) then
         if Base_Aspect (A) then
            Owner := Base_Type (Owner);
         end if;

         if Is_Class_Wide_Type (Owner) and then Inherited_Aspect (A) then
            Owner := Root_Type (Owner);
         end if;

         if Is_Private_Type (Owner)
           and then Present (Full_View (Owner))
           and then not Operational_Aspect (A)
         then
            Owner := Full_View (Owner);
         end if;
      end if;

      --  Search the representation items for the desired aspect

      Item := First_Rep_Item (Owner);
      while Present (Item) loop
         if Nkind (Item) = N_Aspect_Specification
           and then Get_Aspect_Id (Item) = A
           and then Class_Present = Sinfo.Nodes.Class_Present (Item)
         then
            return Item;

         --  We could do something similar here for an N_Pragma node
         --  when Get_Aspect_Id (Pragma_Name (Item)) = A, but let's
         --  wait for a demonstrated need.

         elsif Or_Rep_Item
           and then not Class_Present
           and then Nkind (Item) = N_Attribute_Definition_Clause
           and then Get_Aspect_Id (Chars (Item)) = A
         then
            --  Remember this candidate in case we don't find anything better
            Alternative_Rep_Item := Item;
         end if;

         Next_Rep_Item (Item);
      end loop;

      --  Note that not all aspects are added to the chain of representation
      --  items. In such cases, search the list of aspect specifications. First
      --  find the declaration node where the aspects reside. This is usually
      --  the parent or the parent of the parent.

      if No (Parent (Owner)) then
         return Empty;
      end if;

      Decl := Parent (Owner);
      if not Permits_Aspect_Specifications (Decl) then
         Decl := Parent (Decl);

         if No (Decl) then
            --  Perhaps this happens because the tree is under construction
            --  and Parent (Decl) has not been set yet?

            return Empty;
         end if;
      end if;

      --  Search the list of aspect specifications for the desired aspect

      if Permits_Aspect_Specifications (Decl) then
         Spec := First (Aspect_Specifications (Decl));
         while Present (Spec) loop
            if Get_Aspect_Id (Spec) = A
              and then Class_Present = Sinfo.Nodes.Class_Present (Spec)
            then
               return Spec;
            end if;

            declare
               use User_Aspect_Support;
            begin
               if Get_Aspect_Id (Spec) = Aspect_User_Aspect
                  and then not Analyzed (Spec)
                  and then
                    Analyze_User_Aspect_Aspect_Specification_Hook /= null
               then
                  Analyze_User_Aspect_Aspect_Specification_Hook.all (Spec);
               end if;
            end;

            Next (Spec);
         end loop;
      end if;

      --  The entity does not carry any aspects or the desired aspect was not
      --  found. We have no N_Aspect_Specification node to return, but
      --  Alternative_Rep_Item may have been set (if Or_Rep_Item is True).

      return Alternative_Rep_Item;
   end Find_Aspect;

   --------------------------
   -- Find_Value_Of_Aspect --
   --------------------------

   function Find_Value_Of_Aspect
     (Id            : Entity_Id;
      A             : Aspect_Id;
      Class_Present : Boolean := False) return Node_Id
   is
      Spec : constant Node_Id := Find_Aspect (Id, A,
                                              Class_Present => Class_Present);

   begin
      if Present (Spec) then
         if A = Aspect_Default_Iterator
           and then Present (Aspect_Rep_Item (Spec))
         then
            return Expression (Aspect_Rep_Item (Spec));
         else
            return Expression (Spec);
         end if;
      end if;

      return Empty;
   end Find_Value_Of_Aspect;

   -------------------
   -- Get_Aspect_Id --
   -------------------

   function Get_Aspect_Id (Name : Name_Id) return Aspect_Id is
   begin
      return Aspect_Id_Hash_Table.Get (Name);
   end Get_Aspect_Id;

   function Get_Aspect_Id (Aspect : Node_Id) return Aspect_Id is
   begin
      pragma Assert (Nkind (Aspect) = N_Aspect_Specification);
      return Aspect_Id_Hash_Table.Get (Chars (Identifier (Aspect)));
   end Get_Aspect_Id;

   ----------------
   -- Has_Aspect --
   ----------------

   function Has_Aspect
     (Id            : Entity_Id;
      A             : Aspect_Id;
      Class_Present : Boolean := False) return Boolean
   is
   begin
      return Present (Find_Aspect (Id, A, Class_Present => Class_Present));
   end Has_Aspect;

   function Has_Aspects (N : Node_Id) return Boolean
   is (Atree.Present (N) and then
       Permits_Aspect_Specifications (N) and then
       Nlists.Present (Sinfo.Nodes.Aspect_Specifications (N)) and then
       Nlists.Is_Non_Empty_List (Sinfo.Nodes.Aspect_Specifications (N)));

   ------------------
   -- Is_Aspect_Id --
   ------------------

   function Is_Aspect_Id (Aspect : Name_Id) return Boolean is
     (Get_Aspect_Id (Aspect) /= No_Aspect);

   function Is_Aspect_Id (Aspect : Node_Id) return Boolean is
     (Get_Aspect_Id (Aspect) /= No_Aspect);

   ------------------
   -- Move_Aspects --
   ------------------

   procedure Move_Aspects (From : Node_Id; To : Node_Id) is
      pragma Assert (not Has_Aspects (To));
   begin
      if Has_Aspects (From) then
         Set_Aspect_Specifications (To, Aspect_Specifications (From));
         Set_Aspect_Specifications (From, No_List);
      end if;
   end Move_Aspects;

   ---------------------------
   -- Move_Or_Merge_Aspects --
   ---------------------------

   procedure Move_Or_Merge_Aspects (From : Node_Id; To : Node_Id) is
      procedure Relocate_Aspect (Asp : Node_Id);
      --  Move aspect specification Asp to the aspect specifications of node To

      ---------------------
      -- Relocate_Aspect --
      ---------------------

      procedure Relocate_Aspect (Asp : Node_Id) is
         Asps : List_Id;

      begin
         if Has_Aspects (To) then
            Asps := Aspect_Specifications (To);

         --  Create a new aspect specification list for node To

         else
            Asps := New_List;
            Set_Aspect_Specifications (To, Asps);
         end if;

         --  Remove the aspect from its original owner and relocate it to node
         --  To.

         Remove (Asp);
         Append (Asp, Asps);
      end Relocate_Aspect;

      --  Local variables

      Asp      : Node_Id;
      Asp_Id   : Aspect_Id;
      Next_Asp : Node_Id;

   --  Start of processing for Move_Or_Merge_Aspects

   begin
      if Has_Aspects (From) then
         Asp := First (Aspect_Specifications (From));
         while Present (Asp) loop

            --  Store the next aspect now as a potential relocation will alter
            --  the contents of the list.

            Next_Asp := Next (Asp);

            --  When moving or merging aspects from a subprogram body stub that
            --  also acts as a spec, relocate only those aspects that may apply
            --  to a body [stub]. Note that a precondition must also be moved
            --  to the proper body as the pre/post machinery expects it to be
            --  there.

            if Nkind (From) = N_Subprogram_Body_Stub
              and then No (Corresponding_Spec_Of_Stub (From))
            then
               Asp_Id := Get_Aspect_Id (Asp);

               if Aspect_On_Body_Or_Stub_OK (Asp_Id)
                 or else Asp_Id = Aspect_Pre
                 or else Asp_Id = Aspect_Precondition
               then
                  Relocate_Aspect (Asp);
               end if;

            --  When moving or merging aspects from a single concurrent type
            --  declaration, relocate only those aspects that may apply to the
            --  anonymous object created for the type.

            --  Note: It is better to use Is_Single_Concurrent_Type_Declaration
            --  here, but Aspects and Sem_Util have incompatible licenses.

            elsif Nkind (Original_Node (From)) in
                    N_Single_Protected_Declaration | N_Single_Task_Declaration
            then
               Asp_Id := Get_Aspect_Id (Asp);

               if Aspect_On_Anonymous_Object_OK (Asp_Id) then
                  Relocate_Aspect (Asp);
               end if;

            --  Default case - relocate the aspect to its new owner

            else
               Relocate_Aspect (Asp);
            end if;

            Asp := Next_Asp;
         end loop;

         --  The relocations may have left node From's aspect specifications
         --  list empty. If this is the case, simply remove the aspects.

         if Is_Empty_List (Aspect_Specifications (From)) then
            Remove_Aspects (From);
         end if;
      end if;
   end Move_Or_Merge_Aspects;

   -------------------
   --  Copy_Aspects --
   -------------------

   procedure Copy_Aspects (From : Node_Id; To : Node_Id) is

   begin
      if not Has_Aspects (From) then
         return;
      end if;

      Set_Aspect_Specifications
         (To, New_Copy_List (Aspect_Specifications (From)));
   end Copy_Aspects;

   -----------------------------------
   -- Permits_Aspect_Specifications --
   -----------------------------------

   Has_Aspect_Specifications_Flag : constant array (Node_Kind) of Boolean :=
     (N_Abstract_Subprogram_Declaration        => True,
      N_Component_Declaration                  => True,
      N_Entry_Body                             => True,
      N_Entry_Declaration                      => True,
      N_Exception_Declaration                  => True,
      N_Exception_Renaming_Declaration         => True,
      N_Expression_Function                    => True,
      N_Formal_Abstract_Subprogram_Declaration => True,
      N_Formal_Concrete_Subprogram_Declaration => True,
      N_Formal_Object_Declaration              => True,
      N_Formal_Package_Declaration             => True,
      N_Formal_Type_Declaration                => True,
      N_Full_Type_Declaration                  => True,
      N_Function_Instantiation                 => True,
      N_Generic_Package_Declaration            => True,
      N_Generic_Renaming_Declaration           => True,
      N_Generic_Subprogram_Declaration         => True,
      N_Object_Declaration                     => True,
      N_Object_Renaming_Declaration            => True,
      N_Package_Body                           => True,
      N_Package_Body_Stub                      => True,
      N_Package_Declaration                    => True,
      N_Package_Instantiation                  => True,
      N_Package_Specification                  => True,
      N_Package_Renaming_Declaration           => True,
      N_Parameter_Specification                => True,
      N_Private_Extension_Declaration          => True,
      N_Private_Type_Declaration               => True,
      N_Procedure_Instantiation                => True,
      N_Protected_Body                         => True,
      N_Protected_Body_Stub                    => True,
      N_Protected_Type_Declaration             => True,
      N_Single_Protected_Declaration           => True,
      N_Single_Task_Declaration                => True,
      N_Subprogram_Body                        => True,
      N_Subprogram_Body_Stub                   => True,
      N_Subprogram_Declaration                 => True,
      N_Subprogram_Renaming_Declaration        => True,
      N_Subtype_Declaration                    => True,
      N_Task_Body                              => True,
      N_Task_Body_Stub                         => True,
      N_Task_Type_Declaration                  => True,
      others                                   => False);

   function Permits_Aspect_Specifications (N : Node_Id) return Boolean is
   begin
      pragma Assert (Present (N));
      return Has_Aspect_Specifications_Flag (Nkind (N));
   end Permits_Aspect_Specifications;

   --------------------
   -- Remove_Aspects --
   --------------------

   procedure Remove_Aspects (N : Node_Id) is
   begin
      if Has_Aspects (N) then
         Set_Aspect_Specifications (N, No_List);
      end if;
   end Remove_Aspects;

   -----------------
   -- Same_Aspect --
   -----------------

   --  Table used for Same_Aspect, maps aspect to canonical aspect

   type Aspect_To_Aspect_Mapping is array (Aspect_Id) of Aspect_Id;

   function Init_Canonical_Aspect return Aspect_To_Aspect_Mapping;
   --  Initialize the Canonical_Aspect mapping below

   function Init_Canonical_Aspect return Aspect_To_Aspect_Mapping is
      Result : Aspect_To_Aspect_Mapping;
   begin
      --  They all map to themselves...

      for Aspect in Aspect_Id loop
         Result (Aspect) := Aspect;
      end loop;

      --  ...except for these:

      Result (Aspect_Dynamic_Predicate)  := Aspect_Predicate;
      Result (Aspect_Ghost_Predicate)    := Aspect_Predicate;
      Result (Aspect_Inline_Always)      := Aspect_Inline;
      Result (Aspect_Interrupt_Priority) := Aspect_Priority;
      Result (Aspect_Postcondition)      := Aspect_Post;
      Result (Aspect_Precondition)       := Aspect_Pre;
      Result (Aspect_Shared)             := Aspect_Atomic;
      Result (Aspect_Static_Predicate)   := Aspect_Predicate;
      Result (Aspect_Type_Invariant)     := Aspect_Invariant;

      return Result;
   end Init_Canonical_Aspect;

   Canonical_Aspect : constant Aspect_To_Aspect_Mapping :=
     Init_Canonical_Aspect;

   function Same_Aspect (A1 : Aspect_Id; A2 : Aspect_Id) return Boolean is
   begin
      return Canonical_Aspect (A1) = Canonical_Aspect (A2);
   end Same_Aspect;

   package body User_Aspect_Support is

      --  This is similar to the way that user-defined check names are
      --  managed via package Checks.Check_Names; simple global state.

      UAD_Pragma_Map_Size : constant := 511;

      subtype UAD_Pragma_Map_Header is
        Integer range 0 .. UAD_Pragma_Map_Size - 1;

      function UAD_Pragma_Map_Hash (Chars : Name_Id)
        return UAD_Pragma_Map_Header
      is (UAD_Pragma_Map_Header (Chars mod UAD_Pragma_Map_Size));

      package UAD_Pragma_Map is new GNAT.Htable.Simple_Htable
        (Header_Num => UAD_Pragma_Map_Header,
         Key        => Name_Id,
         Element    => Opt_N_Pragma_Id,
         No_Element => Empty,
         Hash       => UAD_Pragma_Map_Hash,
         Equal      => "=");

      procedure Register_UAD_Pragma (UAD_Pragma : Node_Id) is
         Aspect_Name : constant Name_Id :=
           Chars (Expression
                    (First (Pragma_Argument_Associations (UAD_Pragma))));
      begin
         UAD_Pragma_Map.Set (Aspect_Name, UAD_Pragma);
      end Register_UAD_Pragma;

      function Registered_UAD_Pragma (Aspect_Name : Name_Id) return Node_Id is
      begin
         return UAD_Pragma_Map.Get (Aspect_Name);
      end Registered_UAD_Pragma;
   end User_Aspect_Support;

--  Package initialization sets up Aspect Id hash table

begin
   for J in Aspect_Id loop
      Aspect_Id_Hash_Table.Set (Aspect_Names (J), J);
   end loop;
end Aspects;
