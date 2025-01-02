------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--            I N T E R F A C E S . C H E R I . E X C E P T I O N S         --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--                     Copyright (C) 2024-2025, AdaCore                     --
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
with Ada.Exceptions;

--  This package defines exception types for CHERI-related errors

package Interfaces.CHERI.Exceptions with
  Preelaborate
is

   Capability_Bound_Error : exception;
   --  An out-of-bounds access was attempted

   Capability_Permission_Error : exception;
   --  An attempted access exceeded the permissions granted by a capability

   Capability_Sealed_Error : exception;
   --  A sealed capability was dereferenced

   Capability_Tag_Error : exception;
   --  An invalid capability was dereferenced

private

   --  Expose C names for exception identifiers to allow raising from signal
   --  handlers in init.c.

   Capability_Bound_Error_Id : constant Ada.Exceptions.Exception_Id :=
                                 Capability_Bound_Error'Identity;
   pragma Export (C,
                  Capability_Bound_Error_Id,
                  "capability_bound_error_id");

   Capability_Permission_Error_Id : constant Ada.Exceptions.Exception_Id :=
                                 Capability_Permission_Error'Identity;
   pragma Export (C,
                  Capability_Permission_Error_Id,
                  "capability_permission_error_id");

   Capability_Sealed_Error_Id : constant Ada.Exceptions.Exception_Id :=
                                 Capability_Sealed_Error'Identity;
   pragma Export (C,
                  Capability_Sealed_Error_Id,
                  "capability_sealed_error_id");

   Capability_Tag_Error_Id : constant Ada.Exceptions.Exception_Id :=
                                 Capability_Tag_Error'Identity;
   pragma Export (C,
                  Capability_Tag_Error_Id,
                  "capability_tag_error_id");

end Interfaces.CHERI.Exceptions;
