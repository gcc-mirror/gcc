------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                S T A N D                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--     Copyright (C) 1992,1993,1994,1995 Free Software Foundation, Inc.     --
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

with System;  use System;
with Tree_IO; use Tree_IO;

package body Stand is

   ---------------
   -- Tree_Read --
   ---------------

   procedure Tree_Read is
   begin
      Tree_Read_Data (Standard_Entity'Address,
                       Standard_Entity_Array_Type'Size / Storage_Unit);

      Tree_Read_Int (Int (Standard_Package_Node));
      Tree_Read_Int (Int (Last_Standard_Node_Id));
      Tree_Read_Int (Int (Last_Standard_List_Id));
      Tree_Read_Int (Int (Standard_Void_Type));
      Tree_Read_Int (Int (Standard_Exception_Type));
      Tree_Read_Int (Int (Standard_A_String));
      Tree_Read_Int (Int (Any_Id));
      Tree_Read_Int (Int (Any_Type));
      Tree_Read_Int (Int (Any_Access));
      Tree_Read_Int (Int (Any_Array));
      Tree_Read_Int (Int (Any_Boolean));
      Tree_Read_Int (Int (Any_Character));
      Tree_Read_Int (Int (Any_Composite));
      Tree_Read_Int (Int (Any_Discrete));
      Tree_Read_Int (Int (Any_Fixed));
      Tree_Read_Int (Int (Any_Integer));
      Tree_Read_Int (Int (Any_Numeric));
      Tree_Read_Int (Int (Any_Real));
      Tree_Read_Int (Int (Any_Scalar));
      Tree_Read_Int (Int (Any_String));
      Tree_Read_Int (Int (Universal_Integer));
      Tree_Read_Int (Int (Universal_Real));
      Tree_Read_Int (Int (Universal_Fixed));
      Tree_Read_Int (Int (Standard_Integer_8));
      Tree_Read_Int (Int (Standard_Integer_16));
      Tree_Read_Int (Int (Standard_Integer_32));
      Tree_Read_Int (Int (Standard_Integer_64));
      Tree_Read_Int (Int (Abort_Signal));
      Tree_Read_Int (Int (Standard_Op_Rotate_Left));
      Tree_Read_Int (Int (Standard_Op_Rotate_Right));
      Tree_Read_Int (Int (Standard_Op_Shift_Left));
      Tree_Read_Int (Int (Standard_Op_Shift_Right));
      Tree_Read_Int (Int (Standard_Op_Shift_Right_Arithmetic));

   end Tree_Read;

   ----------------
   -- Tree_Write --
   ----------------

   procedure Tree_Write is
   begin
      Tree_Write_Data (Standard_Entity'Address,
                       Standard_Entity_Array_Type'Size / Storage_Unit);

      Tree_Write_Int (Int (Standard_Package_Node));
      Tree_Write_Int (Int (Last_Standard_Node_Id));
      Tree_Write_Int (Int (Last_Standard_List_Id));
      Tree_Write_Int (Int (Standard_Void_Type));
      Tree_Write_Int (Int (Standard_Exception_Type));
      Tree_Write_Int (Int (Standard_A_String));
      Tree_Write_Int (Int (Any_Id));
      Tree_Write_Int (Int (Any_Type));
      Tree_Write_Int (Int (Any_Access));
      Tree_Write_Int (Int (Any_Array));
      Tree_Write_Int (Int (Any_Boolean));
      Tree_Write_Int (Int (Any_Character));
      Tree_Write_Int (Int (Any_Composite));
      Tree_Write_Int (Int (Any_Discrete));
      Tree_Write_Int (Int (Any_Fixed));
      Tree_Write_Int (Int (Any_Integer));
      Tree_Write_Int (Int (Any_Numeric));
      Tree_Write_Int (Int (Any_Real));
      Tree_Write_Int (Int (Any_Scalar));
      Tree_Write_Int (Int (Any_String));
      Tree_Write_Int (Int (Universal_Integer));
      Tree_Write_Int (Int (Universal_Real));
      Tree_Write_Int (Int (Universal_Fixed));
      Tree_Write_Int (Int (Standard_Integer_8));
      Tree_Write_Int (Int (Standard_Integer_16));
      Tree_Write_Int (Int (Standard_Integer_32));
      Tree_Write_Int (Int (Standard_Integer_64));
      Tree_Write_Int (Int (Abort_Signal));
      Tree_Write_Int (Int (Standard_Op_Rotate_Left));
      Tree_Write_Int (Int (Standard_Op_Rotate_Right));
      Tree_Write_Int (Int (Standard_Op_Shift_Left));
      Tree_Write_Int (Int (Standard_Op_Shift_Right));
      Tree_Write_Int (Int (Standard_Op_Shift_Right_Arithmetic));

   end Tree_Write;

end Stand;
