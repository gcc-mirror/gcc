------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                      G N A T . D E B U G _ P O O L S                     --
--                                                                          --
--                                B o d y                                   --
--                                                                          --
--                            $Revision: 1.1 $
--                                                                          --
--          Copyright (C) 1992-2001 Free Software Foundation, Inc.          --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
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

with Unchecked_Conversion;
with GNAT.HTable;
with System.Memory;

pragma Elaborate_All (GNAT.HTable);

package body GNAT.Debug_Pools is
   use System;
   use System.Memory;
   use System.Storage_Elements;

   --  Definition of a H-table storing the status of each storage chunck
   --  used by this pool

   type State is (Not_Allocated, Deallocated, Allocated);

   type Header is range 1 .. 1023;
   function H (F : Address) return Header;

   package Table is new GNAT.HTable.Simple_HTable (
     Header_Num => Header,
     Element    => State,
     No_Element => Not_Allocated,
     Key        => Address,
     Hash       => H,
     Equal      => "=");

   --------------
   -- Allocate --
   --------------

   procedure Allocate
     (Pool                     : in out Debug_Pool;
      Storage_Address          : out Address;
      Size_In_Storage_Elements : Storage_Count;
      Alignment                : Storage_Count) is
   begin
      Storage_Address := Alloc (size_t (Size_In_Storage_Elements));

      if Storage_Address = Null_Address then
         raise Storage_Error;
      else
         Table.Set (Storage_Address, Allocated);
         Pool.Allocated := Pool.Allocated + Size_In_Storage_Elements;

         if Pool.Allocated - Pool.Deallocated >  Pool.High_Water then
            Pool.High_Water := Pool.Allocated - Pool.Deallocated;
         end if;
      end if;
   end Allocate;

   ----------------
   -- Deallocate --
   ----------------

   procedure Deallocate
     (Pool                     : in out Debug_Pool;
      Storage_Address          : Address;
      Size_In_Storage_Elements : Storage_Count;
      Alignment                : Storage_Count)
   is
      procedure Free (Address : System.Address; Siz : Storage_Count);
      --  Faked free, that reset all the deallocated storage to "DEADBEEF"

      procedure Free (Address : System.Address; Siz : Storage_Count) is
         DB1 : constant Integer := 16#DEAD#;
         DB2 : constant Integer := 16#BEEF#;

         type Dead_Memory is array (1 .. Siz / 4) of Integer;
         type Mem_Ptr is access all Dead_Memory;

         function From_Ptr is
           new Unchecked_Conversion (System.Address, Mem_Ptr);

         J : Storage_Offset;

      begin
         J := Dead_Memory'First;
         while J < Dead_Memory'Last loop
            From_Ptr (Address) (J) := DB1;
            From_Ptr (Address) (J + 1) := DB2;
            J := J + 2;
         end loop;

         if J = Dead_Memory'Last then
            From_Ptr (Address) (J) := DB1;
         end if;
      end Free;

      S : State := Table.Get (Storage_Address);

   --  Start of processing for Deallocate

   begin
      case S is
         when Not_Allocated =>
            raise Freeing_Not_Allocated_Storage;

         when Deallocated   =>
            raise  Freeing_Deallocated_Storage;

         when Allocated =>
            Free (Storage_Address, Size_In_Storage_Elements);
            Table.Set (Storage_Address, Deallocated);
            Pool.Deallocated := Pool.Deallocated + Size_In_Storage_Elements;
      end case;
   end Deallocate;

   -----------------
   -- Dereference --
   -----------------

   procedure Dereference
     (Pool                     : in out Debug_Pool;
      Storage_Address          : Address;
      Size_In_Storage_Elements : Storage_Count;
      Alignment                : Storage_Count)
   is
      S       : State := Table.Get (Storage_Address);
      Max_Dim : constant := 3;
      Dim     : Integer  := 1;

   begin

      --  If this is not a known address, maybe it is because is is an
      --  unconstained array. In which case, the bounds have used the
      --  2 first words (per dimension) of the allocated spot.

      while S = Not_Allocated and then Dim <= Max_Dim loop
         S := Table.Get (Storage_Address - Storage_Offset (Dim * 2 * 4));
         Dim := Dim + 1;
      end loop;

      case S is
         when  Not_Allocated =>
            raise Accessing_Not_Allocated_Storage;

         when Deallocated =>
            raise Accessing_Deallocated_Storage;

         when Allocated =>
            null;
      end case;
   end Dereference;

   -------
   -- H --
   -------

   function H (F : Address) return Header is
   begin
      return
        Header (1 + (To_Integer (F) mod Integer_Address (Header'Last)));
   end H;

   ----------------
   -- Print_Info --
   ----------------

   procedure Print_Info (Pool : Debug_Pool) is
      use System.Storage_Elements;

   begin
      Put_Line ("Debug Pool info:");
      Put_Line ("  Total allocated bytes : "
        & Storage_Offset'Image (Pool.Allocated));

      Put_Line ("  Total deallocated bytes : "
        & Storage_Offset'Image (Pool.Deallocated));

      Put_Line ("  Current Water Mark: "
        & Storage_Offset'Image (Pool.Allocated - Pool.Deallocated));

      Put_Line ("  High Water Mark: "
        & Storage_Offset'Image (Pool.High_Water));
      Put_Line ("");
   end Print_Info;

   ------------------
   -- Storage_Size --
   ------------------

   function Storage_Size (Pool : Debug_Pool) return Storage_Count is
   begin
      return Storage_Count'Last;
   end Storage_Size;

end GNAT.Debug_Pools;
