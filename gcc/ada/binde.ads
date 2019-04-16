------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                B I N D E                                 --
--                                                                          --
--                                 S p e c                                  --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This package contains the routine that determines library-level elaboration
--  order.

with ALI;   use ALI;
with Namet; use Namet;
with Types; use Types;

with GNAT.Dynamic_Tables;

package Binde is

   package Unit_Id_Tables is new GNAT.Dynamic_Tables
     (Table_Component_Type => Unit_Id,
      Table_Index_Type     => Nat,
      Table_Low_Bound      => 1,
      Table_Initial        => 500,
      Table_Increment      => 200);
   use Unit_Id_Tables;

   subtype Unit_Id_Table is Unit_Id_Tables.Instance;
   subtype Unit_Id_Array is Unit_Id_Tables.Table_Type;

   procedure Find_Elab_Order
     (Elab_Order          : out Unit_Id_Table;
      First_Main_Lib_File : File_Name_Type);
   --  Determine elaboration order.
   --
   --  The Elab_Order table records the chosen elaboration order. It is used by
   --  Gen_Elab_Calls to generate the sequence of elaboration calls. Note that
   --  units are included in this table even if they have no elaboration
   --  routine, since the table is also used to drive the generation of object
   --  files in the binder output. Gen_Elab_Calls skips any units that have no
   --  elaboration routine.

end Binde;
