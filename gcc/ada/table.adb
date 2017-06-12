------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                T A B L E                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2017, Free Software Foundation, Inc.         --
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

with System;  use System;
with Tree_IO; use Tree_IO;

with System.Memory; use System.Memory;

with Unchecked_Conversion;

package body Table is
   package body Table is

      function Tree_Get_Table_Address return Address;
      --  Return Null_Address if the table length is zero,
      --  Table (First)'Address if not.

      ----------------------------
      -- Tree_Get_Table_Address --
      ----------------------------

      function Tree_Get_Table_Address return Address is
      begin
         if Is_Empty then
            return Null_Address;
         else
            return Table (First)'Address;
         end if;
      end Tree_Get_Table_Address;

      ---------------
      -- Tree_Read --
      ---------------

      --  Note: we allocate only the space required to accommodate the data
      --  actually written, which means that a Tree_Write/Tree_Read sequence
      --  does an implicit Release.

      procedure Tree_Read is
         Last : Int;
      begin
         Init;
         Tree_Read_Int (Last);
         Set_Last (Table_Last_Type (Last));

         Tree_Read_Data
           (Tree_Get_Table_Address,
             (Last - Int (First) + 1) *

               --  Note the importance of parenthesizing the following division
               --  to avoid the possibility of intermediate overflow.

               (Table_Type'Component_Size / Storage_Unit));
      end Tree_Read;

      ----------------
      -- Tree_Write --
      ----------------

      --  Note: we write out only the currently valid data, not the entire
      --  contents of the allocated array. See note above on Tree_Read.

      procedure Tree_Write is
      begin
         Tree_Write_Int (Int (Last));
         Tree_Write_Data
           (Tree_Get_Table_Address,
            (Int (Last - First) + 1) *
              (Table_Type'Component_Size / Storage_Unit));
      end Tree_Write;

   end Table;
end Table;
