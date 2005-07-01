------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--            S Y S T E M . V M S _ E X C E P T I O N _ T A B L E           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--           Copyright (C) 1997-2004 Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
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

--  This package is usually used only on Alpha/VMS systems in the case
--  where there is at least one Import/Export exception present.

with System.Standard_Library;

package System.VMS_Exception_Table is

   package SSL renames System.Standard_Library;

   procedure Register_VMS_Exception
     (Code : SSL.Exception_Code;
      E    : SSL.Exception_Data_Ptr);
   --  Register an exception in the hash table mapping with a VMS
   --  condition code.

   --  LOTS more comments needed here regarding the enire scheme ???

private

   function Base_Code_In (Code : SSL.Exception_Code) return SSL.Exception_Code;
   --  Value of Code with the severity bits masked off.

   function Coded_Exception (X : SSL.Exception_Code)
     return SSL.Exception_Data_Ptr;
   --  Given a VMS condition, find and return it's allocated Ada exception
   --  (called only from init.c).

end System.VMS_Exception_Table;
