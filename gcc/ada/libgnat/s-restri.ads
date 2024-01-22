------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                  S Y S T E M . R E S T R I C T I O N S                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2004-2024, Free Software Foundation, Inc.         --
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

--  This package provides a run-time interface for checking the set of
--  restrictions that applies to the current partition. The information
--  comes both from explicit restriction pragmas present, and also from
--  compile time checking.

--  The package simply contains an instantiation of System.Rident, but
--  with names discarded, so that we do not have image tables for the
--  large restriction enumeration types at run time.

with System.Rident;

package System.Restrictions is
   pragma Preelaborate;

   pragma Discard_Names;
   package Rident is new System.Rident;
   --  Instantiate a copy of System.Rident without enumeration image names

   Run_Time_Restrictions : Rident.Restrictions_Info;
   --  Restrictions as set by the user, or detected by the binder. See details
   --  in package System.Rident for what restrictions are included in the list
   --  and the format of the information.
   --
   --  Note that a restriction which is both Set and Violated at run-time means
   --  that the violation was detected as part of the Ada run-time and not as
   --  part of user code.

   ------------------
   -- Subprograms --
   -----------------

   function Abort_Allowed return Boolean;
   pragma Inline (Abort_Allowed);
   --  Tests to see if abort is allowed by the current restrictions settings.
   --  For abort to be allowed, either No_Abort_Statements must be False, or
   --  Max_Asynchronous_Select_Nesting must be non-zero.

   function Tasking_Allowed return Boolean;
   pragma Inline (Tasking_Allowed);
   --  Tests to see if tasking operations are allowed by the current
   --  restrictions settings. For tasking to be allowed, No_Tasking must
   --  be False, and Max_Tasks must not be set to zero.

end System.Restrictions;
