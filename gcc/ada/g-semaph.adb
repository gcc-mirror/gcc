------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                      G N A T . S E M A P H O R E S                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--              Copyright (C) 2003 Ada Core Technologies, Inc.              --
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
-- It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). --
--                                                                          --
------------------------------------------------------------------------------

package body GNAT.Semaphores is

   ------------------------
   -- Counting_Semaphore --
   ------------------------

   protected body Counting_Semaphore is

      -----------
      -- Seize --
      -----------

      entry Seize when Count > 0 is
      begin
         Count := Count - 1;
      end Seize;

      -------------
      -- Release --
      -------------

      procedure Release is
      begin
         Count := Count + 1;
      end Release;
   end Counting_Semaphore;

   ----------------------
   -- Binary_Semaphore --
   ----------------------

   protected body Binary_Semaphore is

      -----------
      -- Seize --
      -----------

      entry Seize when Available is
      begin
         Available := False;
      end Seize;

      -------------
      -- Release --
      -------------

      procedure Release is
      begin
         Available := True;
      end Release;
   end Binary_Semaphore;

end GNAT.Semaphores;
