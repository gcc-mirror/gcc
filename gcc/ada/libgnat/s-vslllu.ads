------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                        S Y S T E M . V S _ L L L U                       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2023-2024, Free Software Foundation, Inc.         --
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

--  This package contains specification functions for scanning
--  Long_Long_Long_Unsigned values for use in Text_IO.Modular_IO, and the Value
--  attribute.

--  Preconditions in this unit are meant for analysis only, not for run-time
--  checking, so that the expected exceptions are raised. This is enforced by
--  setting the corresponding assertion policy to Ignore. Postconditions and
--  contract cases should not be executed at runtime as well, in order not to
--  slow down the execution of these functions.

pragma Assertion_Policy (Pre                => Ignore,
                         Post               => Ignore,
                         Contract_Cases     => Ignore,
                         Ghost              => Ignore,
                         Subprogram_Variant => Ignore);

with System.Unsigned_Types;
with System.Value_U_Spec;

package System.Vs_LLLU with SPARK_Mode, Ghost is
   pragma Preelaborate;

   subtype Long_Long_Long_Unsigned is Unsigned_Types.Long_Long_Long_Unsigned;

   package Spec is new System.Value_U_Spec (Long_Long_Long_Unsigned);

end System.Vs_LLLU;
