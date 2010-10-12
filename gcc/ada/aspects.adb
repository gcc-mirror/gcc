------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              A S P E C T S                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2010, Free Software Foundation, Inc.            --
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

with Atree;   use Atree;
with Nlists;  use Nlists;
with Sinfo;   use Sinfo;
with Snames;  use Snames;
with Tree_IO; use Tree_IO;

with GNAT.HTable; use GNAT.HTable;

package body Aspects is

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

   -----------------------------------------
   -- Table Linking Names and Aspect_Id's --
   -----------------------------------------

   type Aspect_Entry is record
      Nam : Name_Id;
      Asp : Aspect_Id;
   end record;

   Aspect_Names : constant array (Integer range <>) of Aspect_Entry := (
     (Name_Ada_2005,                     Aspect_Ada_2005),
     (Name_Ada_2012,                     Aspect_Ada_2012),
     (Name_Address,                      Aspect_Address),
     (Name_Alignment,                    Aspect_Alignment),
     (Name_Atomic,                       Aspect_Atomic),
     (Name_Atomic_Components,            Aspect_Atomic_Components),
     (Name_Bit_Order,                    Aspect_Bit_Order),
     (Name_Component_Size,               Aspect_Component_Size),
     (Name_Discard_Names,                Aspect_Discard_Names),
     (Name_External_Tag,                 Aspect_External_Tag),
     (Name_Favor_Top_Level,              Aspect_Favor_Top_Level),
     (Name_Inline,                       Aspect_Inline),
     (Name_Inline_Always,                Aspect_Inline_Always),
     (Name_Invariant,                    Aspect_Invariant),
     (Name_Machine_Radix,                Aspect_Machine_Radix),
     (Name_Object_Size,                  Aspect_Object_Size),
     (Name_Pack,                         Aspect_Pack),
     (Name_Persistent_BSS,               Aspect_Persistent_BSS),
     (Name_Post,                         Aspect_Post),
     (Name_Pre,                          Aspect_Pre),
     (Name_Predicate,                    Aspect_Predicate),
     (Name_Preelaborable_Initialization, Aspect_Preelaborable_Initialization),
     (Name_Pure_Function,                Aspect_Pure_Function),
     (Name_Shared,                       Aspect_Shared),
     (Name_Size,                         Aspect_Size),
     (Name_Storage_Pool,                 Aspect_Storage_Pool),
     (Name_Storage_Size,                 Aspect_Storage_Size),
     (Name_Stream_Size,                  Aspect_Stream_Size),
     (Name_Suppress,                     Aspect_Suppress),
     (Name_Suppress_Debug_Info,          Aspect_Suppress_Debug_Info),
     (Name_Unchecked_Union,              Aspect_Unchecked_Union),
     (Name_Universal_Aliasing,           Aspect_Universal_Aliasing),
     (Name_Unmodified,                   Aspect_Unmodified),
     (Name_Unreferenced,                 Aspect_Unreferenced),
     (Name_Unreferenced_Objects,         Aspect_Unreferenced_Objects),
     (Name_Unsuppress,                   Aspect_Unsuppress),
     (Name_Value_Size,                   Aspect_Value_Size),
     (Name_Volatile,                     Aspect_Volatile),
     (Name_Volatile_Components,          Aspect_Volatile_Components),
     (Name_Warnings,                     Aspect_Warnings));

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

   -------------------
   -- Get_Aspect_Id --
   -------------------

   function Get_Aspect_Id (Name : Name_Id) return Aspect_Id is
   begin
      return Aspect_Id_Hash_Table.Get (Name);
   end Get_Aspect_Id;

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
      N_Formal_Abstract_Subprogram_Declaration => True,
      N_Formal_Concrete_Subprogram_Declaration => True,
      N_Formal_Object_Declaration              => True,
      N_Formal_Package_Declaration             => True,
      N_Formal_Type_Declaration                => True,
      N_Full_Type_Declaration                  => True,
      N_Function_Instantiation                 => True,
      N_Generic_Package_Declaration            => True,
      N_Generic_Subprogram_Declaration         => True,
      N_Object_Declaration                     => True,
      N_Package_Declaration                    => True,
      N_Package_Instantiation                  => True,
      N_Private_Extension_Declaration          => True,
      N_Private_Type_Declaration               => True,
      N_Procedure_Instantiation                => True,
      N_Protected_Type_Declaration             => True,
      N_Single_Protected_Declaration           => True,
      N_Single_Task_Declaration                => True,
      N_Subprogram_Declaration                 => True,
      N_Subtype_Declaration                    => True,
      N_Task_Type_Declaration                  => True,
      others                                   => False);

   function Permits_Aspect_Specifications (N : Node_Id) return Boolean is
   begin
      return Has_Aspect_Specifications_Flag (Nkind (N));
   end Permits_Aspect_Specifications;

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
         Set_Aspect_Specifications (Node, List);
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
   for J in Aspect_Names'Range loop
      Aspect_Id_Hash_Table.Set (Aspect_Names (J).Nam, Aspect_Names (J).Asp);
   end loop;
end Aspects;
