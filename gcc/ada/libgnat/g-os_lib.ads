------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                          G N A T . O S _ L I B                           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1995-2025, Free Software Foundation, Inc.         --
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

--  Operating system interface facilities

--  This package contains types and procedures for interfacing to the
--  underlying OS. It is used by the GNAT compiler and by tools associated
--  with the GNAT compiler, and therefore works for the various operating
--  systems to which GNAT has been ported. This package will undoubtedly grow
--  as new services are needed by various tools.

--  This package tends to use fairly low-level Ada in order to not bring in
--  large portions of the RTL. For example, functions return access to string
--  as part of avoiding functions returning unconstrained types.

--  Except where specifically noted, these routines are portable across all
--  GNAT implementations on all supported operating systems.

--  See file s-os_lib.ads for full documentation of the interface

with System.OS_Lib;

package GNAT.OS_Lib renames System.OS_Lib;
