------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                          G N A T . H T A B L E                           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 1995-2019, AdaCore                     --
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

--  Hash table searching routines

--  This package contains two separate packages. The Simple_HTable package
--  provides a very simple abstraction that associates one element to one
--  key value and takes care of all allocations automatically using the heap.
--  The Static_HTable package provides a more complex interface that allows
--  complete control over allocation.

--  See file s-htable.ads for full documentation of the interface

pragma Compiler_Unit_Warning;

with System.HTable;

package GNAT.HTable is
   pragma Preelaborate;
   pragma Elaborate_Body;
   --  The elaborate body is because we have a dummy body to deal with
   --  bootstrap path problems (we used to have a real body, and now we don't
   --  need it any more, but the bootstrap requires that we have a dummy body,
   --  since otherwise the old body gets picked up; also, we can't use pragma
   --  No_Body because older bootstrap compilers don't support that).

   generic package Simple_HTable renames System.HTable.Simple_HTable;
   generic package Static_HTable renames System.HTable.Static_HTable;

   generic function Hash renames System.HTable.Hash;

end GNAT.HTable;
