------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                      G N A T . S E M A P H O R E S                       --
--                                                                          --
--                                 S p e c                                  --
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
-- It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). --
--                                                                          --
------------------------------------------------------------------------------

--  This package provides classic counting semaphores and binary semaphores.
--  Both types are visibly defined as protected types so that users can make
--  conditional and timed calls when appropriate.

with System;

package GNAT.Semaphores is

   Default_Ceiling : constant System.Priority := System.Default_Priority;
   --  A convenient value for the priority discriminants that follow.

   ------------------------
   -- Counting_Semaphore --
   ------------------------

   protected type Counting_Semaphore
      (Initial_Value : Natural;
      --  A counting semaphore contains an internal counter.  The initial
      --  value of this counter is set by clients via the discriminant.

      Ceiling : System.Priority)
      --  Users must specify the ceiling priority for the object.
      --  If the Real-Time Systems Annex is not in use this value
      --  is not important.
   is
      pragma Priority (Ceiling);

      entry Seize;
      --  Blocks caller until/unless the semaphore's internal counter
      --  is greater than zero.
      --  Decrements the semaphore's internal counter when executed.

      procedure Release;
      --  Increments the semaphore's internal counter.

   private
      Count : Natural := Initial_Value;
   end Counting_Semaphore;

   ----------------------
   -- Binary_Semaphore --
   ----------------------

   protected type Binary_Semaphore
      (Initially_Available : Boolean;
      --  Binary semaphores are either available or not; there is no
      --  internal count involved.  The discriminant value determines
      --  whether the individual object is initially available.
      Ceiling : System.Priority)
      --  Users must specify the ceiling priority for the object.
      --  If the Real-Time Systems Annex is not in use
      --  this value is not important.
   is
      pragma Priority (Ceiling);

      entry Seize;
      --  Blocks the caller unless/until semaphore is available.
      --  After execution the semaphore is no longer available.

      procedure Release;
      --  Makes the semaphore available.

   private
      Available : Boolean := Initially_Available;
   end Binary_Semaphore;

end GNAT.Semaphores;
