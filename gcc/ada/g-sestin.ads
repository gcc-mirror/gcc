------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--            G N A T . S E C O N D A R Y _ S T A C K _ I N F O             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--              Copyright (C) 2004 Ada Core Technologies, Inc.              --
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

--  This package provides facilities for obtaining information on secondary
--  stack usage.

with System.Secondary_Stack;

package GNAT.Secondary_Stack_Info is

   function SS_Get_Max return Long_Long_Integer
     renames System.Secondary_Stack.SS_Get_Max;
   --  Return maximum used space in storage units for the current secondary
   --  stack. For a dynamically allocated secondary stack, the returned
   --  result is always -1. For a statically allocated secondary stack,
   --  the returned value shows the largest amount of space allocated so
   --  far during execution of the program to the current secondary stack,
   --  i.e. the secondary stack for the current task.

end GNAT.Secondary_Stack_Info;
