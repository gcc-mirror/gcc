------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                     A D A . F I N A L I Z A T I O N                      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2014, Free Software Foundation, Inc.         --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
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

pragma Warnings (Off);
with System.Finalization_Root;
pragma Warnings (On);

package Ada.Finalization is
   pragma Pure;

   type Controlled is abstract tagged private;
   pragma Preelaborable_Initialization (Controlled);

   procedure Initialize (Object : in out Controlled);
   procedure Adjust     (Object : in out Controlled);
   procedure Finalize   (Object : in out Controlled);

   type Limited_Controlled is abstract tagged limited private;
   pragma Preelaborable_Initialization (Limited_Controlled);

   procedure Initialize (Object : in out Limited_Controlled);
   procedure Finalize   (Object : in out Limited_Controlled);

private
   package SFR renames System.Finalization_Root;

   type Controlled is abstract new SFR.Root_Controlled with null record;

   --  In order to simplify the implementation, the mechanism in Process_Full_
   --  View ensures that the full view is limited even though the parent type
   --  is not.

   type Limited_Controlled is
     abstract new SFR.Root_Controlled with null record;

end Ada.Finalization;
