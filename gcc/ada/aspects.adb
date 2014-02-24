------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              A S P E C T S                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2010-2013, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Atree;    use Atree;
with Einfo;    use Einfo;
with Nlists;   use Nlists;
with Sinfo;    use Sinfo;
with Tree_IO;  use Tree_IO;

with GNAT.HTable;           use GNAT.HTable;

package body Aspects is

   --  The following array indicates aspects that a subtype inherits from its
   --  base type. True means that the subtype inherits the aspect from its base
   --  type. False means it is not inherited.

   Base_Aspect : constant array (Aspect_Id) of Boolean :=
     (Aspect_Atomic                  => True,
      Aspect_Atomic_Components       => True,
      Aspect_Constant_Indexing       => True,
      Aspect_Default_Iterator        => True,
      Aspect_Discard_Names           => True,
      Aspect_Independent_Components  => True,
      Aspect_Iterator_Element        => True,
      Aspect_Type_Invariant          => True,
      Aspect_Unchecked_Union         => True,
      Aspect_Variable_Indexing       => True,
      Aspect_Volatile                => True,
      others                         => False);

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

   procedure Set_Aspect_Specifications_No_Check (N : Node_Id; L : List_Id);
   --  Same as Set_Aspect_Specifications, but does not contain the assertion
   --  that checks that N does not already have aspect specifications. This
   --  subprogram is supposed to be used as a part of Tree_Read. When reading
   --  tree, first read nodes with their basic properties (as Atree.Tree_Read),
   --  this includes reading the Has_Aspects flag for each node, then we reed
   --  all the list tables and only after that we call Tree_Read for Aspects.
   --  That is, when reading the tree, the list of aspects is attached to the
   --  node that already has Has_Aspects flag set ON.

   ------------------------------------------
   -- Hash Table for Aspect Specifications --
   ------------------------------------------

   type AS_Hash_Range is range 0 .. 510;
   --  Size of hash table headers

   function AS_Hash (F : Node_Id) return AS_Hash_Range;
   --  Hash function for hash table

   function AS_Hash (F : Node_Id) return AS_Hash_Range is
   begin
      return AS_Hash_Range (F mod 511);
   end AS_Hash;

   package Aspect_Specifications_Hash_Table is new
     GNAT.HTable.Simple_HTable
       (Header_Num => AS_Hash_Range,
        Element    => List_Id,
        No_Element => No_List,
        Key        => Node_Id,
        Hash       => AS_Hash,
        Equal      => "=");

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

   ---------------------------
   -- Aspect_Specifications --
   ---------------------------

   function Aspect_Specifications (N : Node_Id) return List_Id is
   begin
      if Has_Aspects (N) then
         return Aspect_Specifications_Hash_Table.Get (N);
      else
         return No_List;
      end if;
   end Aspect_Specifications;

   --------------------------------
   -- Aspects_On_Body_Or_Stub_OK --
   --------------------------------

   function Aspects_On_Body_Or_Stub_OK (N : Node_Id) return Boolean is
      Aspect  : Node_Id;
      Aspects : List_Id;

   begin
      --  The routine should be invoked on a body [stub] with aspects

      pragma Assert (Has_Aspects (N));
      pragma Assert (Nkind (N) in N_Body_Stub
                       or else Nkind_In (N, N_Package_Body,
                                            N_Protected_Body,
                                            N_Subprogram_Body,
                                            N_Task_Body));

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

   ----------------------
   -- Exchange_Aspects --
   ----------------------

   procedure Exchange_Aspects (N1 : Node_Id; N2 : Node_Id) is
   begin
      pragma Assert
        (Permits_Aspect_Specifications (N1)
           and then Permits_Aspect_Specifications (N2));

      --  Perform the exchange only when both nodes have lists to be swapped

      if Has_Aspects (N1) and then Has_Aspects (N2) then
         declare
            L1 : constant List_Id := Aspect_Specifications (N1);
            L2 : constant List_Id := Aspect_Specifications (N2);
         begin
            Set_Parent (L1, N2);
            Set_Parent (L2, N1);
            Aspect_Specifications_Hash_Table.Set (N1, L2);
            Aspect_Specifications_Hash_Table.Set (N2, L1);
         end;
      end if;
   end Exchange_Aspects;

   -----------------
   -- Find_Aspect --
   -----------------

   function Find_Aspect (Id : Entity_Id; A : Aspect_Id) return Node_Id is
      Decl  : Node_Id;
      Item  : Node_Id;
      Owner : Entity_Id;
      Spec  : Node_Id;

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

         if Is_Private_Type (Owner) and then Present (Full_View (Owner)) then
            Owner := Full_View (Owner);
         end if;
      end if;

      --  Search the representation items for the desired aspect

      Item := First_Rep_Item (Owner);
      while Present (Item) loop
         if Nkind (Item) = N_Aspect_Specification
           and then Get_Aspect_Id (Item) = A
         then
            return Item;
         end if;

         Next_Rep_Item (Item);
      end loop;

      --  Note that not all aspects are added to the chain of representation
      --  items. In such cases, search the list of aspect specifications. First
      --  find the declaration node where the aspects reside. This is usually
      --  the parent or the parent of the parent.

      Decl := Parent (Owner);
      if not Permits_Aspect_Specifications (Decl) then
         Decl := Parent (Decl);
      end if;

      --  Search the list of aspect specifications for the desired aspect

      if Permits_Aspect_Specifications (Decl) then
         Spec := First (Aspect_Specifications (Decl));
         while Present (Spec) loop
            if Get_Aspect_Id (Spec) = A then
               return Spec;
            end if;

            Next (Spec);
         end loop;
      end if;

      --  The entity does not carry any aspects or the desired aspect was not
      --  found.

      return Empty;
   end Find_Aspect;

   --------------------------
   -- Find_Value_Of_Aspect --
   --------------------------

   function Find_Value_Of_Aspect
     (Id : Entity_Id;
      A  : Aspect_Id) return Node_Id
   is
      Spec : constant Node_Id := Find_Aspect (Id, A);

   begin
      if Present (Spec) then
         if A = Aspect_Default_Iterator then
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

   function Has_Aspect (Id : Entity_Id; A : Aspect_Id) return Boolean is
   begin
      return Present (Find_Aspect (Id, A));
   end Has_Aspect;

   ------------------
   -- Move_Aspects --
   ------------------

   procedure Move_Aspects (From : Node_Id; To : Node_Id) is
      pragma Assert (not Has_Aspects (To));
   begin
      if Has_Aspects (From) then
         Set_Aspect_Specifications (To, Aspect_Specifications (From));
         Aspect_Specifications_Hash_Table.Remove (From);
         Set_Has_Aspects (From, False);
      end if;
   end Move_Aspects;

   ---------------------------
   -- Move_Or_Merge_Aspects --
   ---------------------------

   procedure Move_Or_Merge_Aspects (From : Node_Id; To : Node_Id) is
      procedure Relocate_Aspect (Asp : Node_Id);
      --  Asp denotes an aspect specification of node From. Relocate the Asp to
      --  the aspect specifications of node To (if any).

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
            Set_Has_Aspects (To);
         end if;

         --  Remove the aspect from node From's aspect specifications and
         --  append it to node To.

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

   -----------------------------------
   -- Permits_Aspect_Specifications --
   -----------------------------------

   Has_Aspect_Specifications_Flag : constant array (Node_Kind) of Boolean :=
     (N_Abstract_Subprogram_Declaration        => True,
      N_Component_Declaration                  => True,
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
      return Has_Aspect_Specifications_Flag (Nkind (N));
   end Permits_Aspect_Specifications;

   --------------------
   -- Remove_Aspects --
   --------------------

   procedure Remove_Aspects (N : Node_Id) is
   begin
      if Has_Aspects (N) then
         Aspect_Specifications_Hash_Table.Remove (N);
         Set_Has_Aspects (N, False);
      end if;
   end Remove_Aspects;

   -----------------
   -- Same_Aspect --
   -----------------

   --  Table used for Same_Aspect, maps aspect to canonical aspect

   Canonical_Aspect : constant array (Aspect_Id) of Aspect_Id :=
   (No_Aspect                           => No_Aspect,
    Aspect_Abstract_State               => Aspect_Abstract_State,
    Aspect_Address                      => Aspect_Address,
    Aspect_Alignment                    => Aspect_Alignment,
    Aspect_All_Calls_Remote             => Aspect_All_Calls_Remote,
    Aspect_Async_Readers                => Aspect_Async_Readers,
    Aspect_Async_Writers                => Aspect_Async_Writers,
    Aspect_Asynchronous                 => Aspect_Asynchronous,
    Aspect_Atomic                       => Aspect_Atomic,
    Aspect_Atomic_Components            => Aspect_Atomic_Components,
    Aspect_Attach_Handler               => Aspect_Attach_Handler,
    Aspect_Bit_Order                    => Aspect_Bit_Order,
    Aspect_Compiler_Unit                => Aspect_Compiler_Unit,
    Aspect_Component_Size               => Aspect_Component_Size,
    Aspect_Constant_Indexing            => Aspect_Constant_Indexing,
    Aspect_Contract_Cases               => Aspect_Contract_Cases,
    Aspect_Convention                   => Aspect_Convention,
    Aspect_CPU                          => Aspect_CPU,
    Aspect_Default_Component_Value      => Aspect_Default_Component_Value,
    Aspect_Default_Iterator             => Aspect_Default_Iterator,
    Aspect_Default_Value                => Aspect_Default_Value,
    Aspect_Depends                      => Aspect_Depends,
    Aspect_Dimension                    => Aspect_Dimension,
    Aspect_Dimension_System             => Aspect_Dimension_System,
    Aspect_Discard_Names                => Aspect_Discard_Names,
    Aspect_Dispatching_Domain           => Aspect_Dispatching_Domain,
    Aspect_Dynamic_Predicate            => Aspect_Predicate,
    Aspect_Effective_Reads              => Aspect_Effective_Reads,
    Aspect_Effective_Writes             => Aspect_Effective_Writes,
    Aspect_Elaborate_Body               => Aspect_Elaborate_Body,
    Aspect_Export                       => Aspect_Export,
    Aspect_External_Name                => Aspect_External_Name,
    Aspect_External_Tag                 => Aspect_External_Tag,
    Aspect_Favor_Top_Level              => Aspect_Favor_Top_Level,
    Aspect_Global                       => Aspect_Global,
    Aspect_Implicit_Dereference         => Aspect_Implicit_Dereference,
    Aspect_Import                       => Aspect_Import,
    Aspect_Independent                  => Aspect_Independent,
    Aspect_Independent_Components       => Aspect_Independent_Components,
    Aspect_Inline                       => Aspect_Inline,
    Aspect_Inline_Always                => Aspect_Inline,
    Aspect_Initial_Condition            => Aspect_Initial_Condition,
    Aspect_Initializes                  => Aspect_Initializes,
    Aspect_Input                        => Aspect_Input,
    Aspect_Interrupt_Handler            => Aspect_Interrupt_Handler,
    Aspect_Interrupt_Priority           => Aspect_Priority,
    Aspect_Invariant                    => Aspect_Invariant,
    Aspect_Iterable                     => Aspect_Iterable,
    Aspect_Iterator_Element             => Aspect_Iterator_Element,
    Aspect_Link_Name                    => Aspect_Link_Name,
    Aspect_Linker_Section               => Aspect_Linker_Section,
    Aspect_Lock_Free                    => Aspect_Lock_Free,
    Aspect_Machine_Radix                => Aspect_Machine_Radix,
    Aspect_No_Return                    => Aspect_No_Return,
    Aspect_Object_Size                  => Aspect_Object_Size,
    Aspect_Output                       => Aspect_Output,
    Aspect_Pack                         => Aspect_Pack,
    Aspect_Part_Of                      => Aspect_Part_Of,
    Aspect_Persistent_BSS               => Aspect_Persistent_BSS,
    Aspect_Post                         => Aspect_Post,
    Aspect_Postcondition                => Aspect_Post,
    Aspect_Pre                          => Aspect_Pre,
    Aspect_Precondition                 => Aspect_Pre,
    Aspect_Predicate                    => Aspect_Predicate,
    Aspect_Preelaborate                 => Aspect_Preelaborate,
    Aspect_Preelaborable_Initialization => Aspect_Preelaborable_Initialization,
    Aspect_Priority                     => Aspect_Priority,
    Aspect_Pure                         => Aspect_Pure,
    Aspect_Pure_Function                => Aspect_Pure_Function,
    Aspect_Refined_Depends              => Aspect_Refined_Depends,
    Aspect_Refined_Global               => Aspect_Refined_Global,
    Aspect_Refined_Post                 => Aspect_Refined_Post,
    Aspect_Refined_State                => Aspect_Refined_State,
    Aspect_Remote_Access_Type           => Aspect_Remote_Access_Type,
    Aspect_Remote_Call_Interface        => Aspect_Remote_Call_Interface,
    Aspect_Remote_Types                 => Aspect_Remote_Types,
    Aspect_Read                         => Aspect_Read,
    Aspect_Relative_Deadline            => Aspect_Relative_Deadline,
    Aspect_Scalar_Storage_Order         => Aspect_Scalar_Storage_Order,
    Aspect_Shared                       => Aspect_Atomic,
    Aspect_Shared_Passive               => Aspect_Shared_Passive,
    Aspect_Simple_Storage_Pool          => Aspect_Simple_Storage_Pool,
    Aspect_Simple_Storage_Pool_Type     => Aspect_Simple_Storage_Pool_Type,
    Aspect_Size                         => Aspect_Size,
    Aspect_Small                        => Aspect_Small,
    Aspect_SPARK_Mode                   => Aspect_SPARK_Mode,
    Aspect_Static_Predicate             => Aspect_Predicate,
    Aspect_Storage_Pool                 => Aspect_Storage_Pool,
    Aspect_Storage_Size                 => Aspect_Storage_Size,
    Aspect_Stream_Size                  => Aspect_Stream_Size,
    Aspect_Suppress                     => Aspect_Suppress,
    Aspect_Suppress_Debug_Info          => Aspect_Suppress_Debug_Info,
    Aspect_Synchronization              => Aspect_Synchronization,
    Aspect_Test_Case                    => Aspect_Test_Case,
    Aspect_Type_Invariant               => Aspect_Invariant,
    Aspect_Unchecked_Union              => Aspect_Unchecked_Union,
    Aspect_Universal_Aliasing           => Aspect_Universal_Aliasing,
    Aspect_Universal_Data               => Aspect_Universal_Data,
    Aspect_Unmodified                   => Aspect_Unmodified,
    Aspect_Unreferenced                 => Aspect_Unreferenced,
    Aspect_Unreferenced_Objects         => Aspect_Unreferenced_Objects,
    Aspect_Unsuppress                   => Aspect_Unsuppress,
    Aspect_Variable_Indexing            => Aspect_Variable_Indexing,
    Aspect_Value_Size                   => Aspect_Value_Size,
    Aspect_Volatile                     => Aspect_Volatile,
    Aspect_Volatile_Components          => Aspect_Volatile_Components,
    Aspect_Warnings                     => Aspect_Warnings,
    Aspect_Write                        => Aspect_Write);

   function Same_Aspect (A1 : Aspect_Id; A2 : Aspect_Id) return Boolean is
   begin
      return Canonical_Aspect (A1) = Canonical_Aspect (A2);
   end Same_Aspect;

   -------------------------------
   -- Set_Aspect_Specifications --
   -------------------------------

   procedure Set_Aspect_Specifications (N : Node_Id; L : List_Id) is
   begin
      pragma Assert (Permits_Aspect_Specifications (N));
      pragma Assert (not Has_Aspects (N));
      pragma Assert (L /= No_List);

      Set_Has_Aspects (N);
      Set_Parent (L, N);
      Aspect_Specifications_Hash_Table.Set (N, L);
   end Set_Aspect_Specifications;

   ----------------------------------------
   -- Set_Aspect_Specifications_No_Check --
   ----------------------------------------

   procedure Set_Aspect_Specifications_No_Check (N : Node_Id; L : List_Id) is
   begin
      pragma Assert (Permits_Aspect_Specifications (N));
      pragma Assert (L /= No_List);

      Set_Has_Aspects (N);
      Set_Parent (L, N);
      Aspect_Specifications_Hash_Table.Set (N, L);
   end Set_Aspect_Specifications_No_Check;

   ---------------
   -- Tree_Read --
   ---------------

   procedure Tree_Read is
      Node : Node_Id;
      List : List_Id;
   begin
      loop
         Tree_Read_Int (Int (Node));
         Tree_Read_Int (Int (List));
         exit when List = No_List;
         Set_Aspect_Specifications_No_Check (Node, List);
      end loop;
   end Tree_Read;

   ----------------
   -- Tree_Write --
   ----------------

   procedure Tree_Write is
      Node : Node_Id := Empty;
      List : List_Id;
   begin
      Aspect_Specifications_Hash_Table.Get_First (Node, List);
      loop
         Tree_Write_Int (Int (Node));
         Tree_Write_Int (Int (List));
         exit when List = No_List;
         Aspect_Specifications_Hash_Table.Get_Next (Node, List);
      end loop;
   end Tree_Write;

--  Package initialization sets up Aspect Id hash table

begin
   for J in Aspect_Id loop
      Aspect_Id_Hash_Table.Set (Aspect_Names (J), J);
   end loop;
end Aspects;
