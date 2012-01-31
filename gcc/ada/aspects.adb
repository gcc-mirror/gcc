------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              A S P E C T S                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2010-2012, Free Software Foundation, Inc.         --
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

   -------------------
   -- Get_Aspect_Id --
   -------------------

   function Get_Aspect_Id (Name : Name_Id) return Aspect_Id is
   begin
      return Aspect_Id_Hash_Table.Get (Name);
   end Get_Aspect_Id;

   -----------------
   -- Find_Aspect --
   -----------------

   function Find_Aspect (Ent : Entity_Id; A : Aspect_Id) return Node_Id is
      Ritem : Node_Id;
      Typ   : Entity_Id;

   begin

      --  If the aspect is an inherited one and the entity is a class-wide
      --  type, use the aspect of the specific type. If the type is a base
      --  aspect, examine the rep. items of the base type.

      if Is_Type (Ent) then
         if Base_Aspect (A) then
            Typ := Base_Type (Ent);
         else
            Typ := Ent;
         end if;

         if Is_Class_Wide_Type (Typ)
           and then Inherited_Aspect (A)
         then
            Ritem := First_Rep_Item (Etype (Typ));
         else
            Ritem := First_Rep_Item (Typ);
         end if;

      else
         Ritem := First_Rep_Item (Ent);
      end if;

      while Present (Ritem) loop
         if Nkind (Ritem) = N_Aspect_Specification
           and then Get_Aspect_Id (Chars (Identifier (Ritem))) = A
         then
            if A = Aspect_Default_Iterator then
               return Expression (Aspect_Rep_Item (Ritem));
            else
               return Expression (Ritem);
            end if;
         end if;

         Next_Rep_Item (Ritem);
      end loop;

      return Empty;
   end Find_Aspect;

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

   -----------------------------------
   -- Permits_Aspect_Specifications --
   -----------------------------------

   Has_Aspect_Specifications_Flag : constant array (Node_Kind) of Boolean :=
     (N_Abstract_Subprogram_Declaration        => True,
      N_Component_Declaration                  => True,
      N_Entry_Declaration                      => True,
      N_Exception_Declaration                  => True,
      N_Exception_Renaming_Declaration         => True,
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
      N_Package_Declaration                    => True,
      N_Package_Instantiation                  => True,
      N_Package_Specification                  => True,
      N_Package_Renaming_Declaration           => True,
      N_Private_Extension_Declaration          => True,
      N_Private_Type_Declaration               => True,
      N_Procedure_Instantiation                => True,
      N_Protected_Body                         => True,
      N_Protected_Type_Declaration             => True,
      N_Single_Protected_Declaration           => True,
      N_Single_Task_Declaration                => True,
      N_Subprogram_Body                        => True,
      N_Subprogram_Declaration                 => True,
      N_Subprogram_Renaming_Declaration        => True,
      N_Subtype_Declaration                    => True,
      N_Task_Body                              => True,
      N_Task_Type_Declaration                  => True,
      others                                   => False);

   function Permits_Aspect_Specifications (N : Node_Id) return Boolean is
   begin
      return Has_Aspect_Specifications_Flag (Nkind (N));
   end Permits_Aspect_Specifications;

   -----------------
   -- Same_Aspect --
   -----------------

   --  Table used for Same_Aspect, maps aspect to canonical aspect

   Canonical_Aspect : constant array (Aspect_Id) of Aspect_Id :=
   (No_Aspect                           => No_Aspect,
    Aspect_Ada_2005                     => Aspect_Ada_2005,
    Aspect_Ada_2012                     => Aspect_Ada_2005,
    Aspect_Address                      => Aspect_Address,
    Aspect_Alignment                    => Aspect_Alignment,
    Aspect_Asynchronous                 => Aspect_Asynchronous,
    Aspect_Atomic                       => Aspect_Atomic,
    Aspect_Atomic_Components            => Aspect_Atomic_Components,
    Aspect_Attach_Handler               => Aspect_Attach_Handler,
    Aspect_Bit_Order                    => Aspect_Bit_Order,
    Aspect_Component_Size               => Aspect_Component_Size,
    Aspect_Constant_Indexing            => Aspect_Constant_Indexing,
    Aspect_CPU                          => Aspect_CPU,
    Aspect_Default_Component_Value      => Aspect_Default_Component_Value,
    Aspect_Default_Iterator             => Aspect_Default_Iterator,
    Aspect_Default_Value                => Aspect_Default_Value,
    Aspect_Dimension                    => Aspect_Dimension,
    Aspect_Dimension_System             => Aspect_Dimension_System,
    Aspect_Discard_Names                => Aspect_Discard_Names,
    Aspect_Dispatching_Domain           => Aspect_Dispatching_Domain,
    Aspect_Dynamic_Predicate            => Aspect_Predicate,
    Aspect_External_Tag                 => Aspect_External_Tag,
    Aspect_Favor_Top_Level              => Aspect_Favor_Top_Level,
    Aspect_Implicit_Dereference         => Aspect_Implicit_Dereference,
    Aspect_Independent                  => Aspect_Independent,
    Aspect_Independent_Components       => Aspect_Independent_Components,
    Aspect_Inline                       => Aspect_Inline,
    Aspect_Inline_Always                => Aspect_Inline,
    Aspect_Interrupt_Handler            => Aspect_Interrupt_Handler,
    Aspect_Interrupt_Priority           => Aspect_Interrupt_Priority,
    Aspect_Iterator_Element             => Aspect_Iterator_Element,
    Aspect_All_Calls_Remote             => Aspect_All_Calls_Remote,
    Aspect_Compiler_Unit                => Aspect_Compiler_Unit,
    Aspect_Elaborate_Body               => Aspect_Elaborate_Body,
    Aspect_Preelaborate                 => Aspect_Preelaborate,
    Aspect_Preelaborate_05              => Aspect_Preelaborate_05,
    Aspect_Pure                         => Aspect_Pure,
    Aspect_Pure_05                      => Aspect_Pure_05,
    Aspect_Pure_12                      => Aspect_Pure_12,
    Aspect_Remote_Call_Interface        => Aspect_Remote_Call_Interface,
    Aspect_Remote_Types                 => Aspect_Remote_Types,
    Aspect_Shared_Passive               => Aspect_Shared_Passive,
    Aspect_Universal_Data               => Aspect_Universal_Data,
    Aspect_Input                        => Aspect_Input,
    Aspect_Invariant                    => Aspect_Invariant,
    Aspect_Machine_Radix                => Aspect_Machine_Radix,
    Aspect_No_Return                    => Aspect_No_Return,
    Aspect_Object_Size                  => Aspect_Object_Size,
    Aspect_Output                       => Aspect_Output,
    Aspect_Pack                         => Aspect_Pack,
    Aspect_Persistent_BSS               => Aspect_Persistent_BSS,
    Aspect_Post                         => Aspect_Post,
    Aspect_Postcondition                => Aspect_Post,
    Aspect_Pre                          => Aspect_Pre,
    Aspect_Precondition                 => Aspect_Pre,
    Aspect_Predicate                    => Aspect_Predicate,
    Aspect_Preelaborable_Initialization => Aspect_Preelaborable_Initialization,
    Aspect_Priority                     => Aspect_Priority,
    Aspect_Pure_Function                => Aspect_Pure_Function,
    Aspect_Remote_Access_Type           => Aspect_Remote_Access_Type,
    Aspect_Read                         => Aspect_Read,
    Aspect_Shared                       => Aspect_Atomic,
    Aspect_Size                         => Aspect_Size,
    Aspect_Small                        => Aspect_Small,
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
