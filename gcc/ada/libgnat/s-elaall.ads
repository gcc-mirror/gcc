------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--         S Y S T E M . E L A B O R A T I O N _ A L L O C A T O R S        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2014-2021, Free Software Foundation, Inc.       --
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

--  This package provides the interfaces for proper handling of restriction
--  No_Standard_Allocators_After_Elaboration. It is used only by programs
--  which use this restriction.

package System.Elaboration_Allocators is
   pragma Preelaborate;

   procedure Mark_Start_Of_Elaboration;
   --  Called right at the start of main elaboration if the program activates
   --  restriction No_Standard_Allocators_After_Elaboration. We don't want to
   --  rely on the normal elaboration mechanism for marking this event, since
   --  that would require us to be sure to elaborate this first, which would
   --  be awkward, and it is convenient to have this package be Preelaborate.

   procedure Mark_End_Of_Elaboration;
   --  Called when main elaboration is complete if the program has activated
   --  restriction No_Standard_Allocators_After_Elaboration. This is the point
   --  beyond which any standard allocator use will violate the restriction.

   procedure Check_Standard_Allocator;
   --  Called as part of every allocator in a program for which the restriction
   --  No_Standard_Allocators_After_Elaboration is active. This will raise an
   --  exception (Storage_Error with an appropriate message) if it is called
   --  after the call to Mark_End_Of_Elaboration.

end System.Elaboration_Allocators;
