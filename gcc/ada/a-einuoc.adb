------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--     A D A . E X C E P T I O N S . I S _ N U L L _ O C C U R R E N C E    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 2000 Free Software Foundation, Inc.            --
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

--  This is a GNAT-specific child function of Ada.Exceptions. It provides
--  clearly missing functionality for its parent package, and most reasonably
--  would simply be an added function to that package, but this change cannot
--  be made in a conforming manner.

function Ada.Exceptions.Is_Null_Occurrence
  (X    : Exception_Occurrence)
   return Boolean
is
begin
   --  The null exception is uniquely identified by the fact that the Id
   --  value is null. No other exception occurrence can have a null Id.

   if X.Id = Null_Id then
      return True;
   else
      return False;
   end if;
end Ada.Exceptions.Is_Null_Occurrence;
