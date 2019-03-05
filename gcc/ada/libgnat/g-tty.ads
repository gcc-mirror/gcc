------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                             G N A T . T T Y                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2002-2019, AdaCore                     --
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

--  This package provides control over pseudo terminals (ttys)

--  This package is only supported on unix systems. See function TTY_Supported
--  to test dynamically whether other functions of this package can be called.

with System;

with GNAT.OS_Lib;

package GNAT.TTY is

   type TTY_Handle is private;
   --  Handle for a tty descriptor

   function TTY_Supported return Boolean;
   --  If True, the other functions of this package can be called. Otherwise,
   --  all functions in this package will raise Program_Error if called.

   procedure Allocate_TTY (Handle : out TTY_Handle);
   --  Allocate a new tty

   procedure Reset_TTY (Handle : TTY_Handle);
   --  Reset settings of a given tty

   procedure Close_TTY (Handle : in out TTY_Handle);
   --  Close a given tty

   function TTY_Name (Handle : TTY_Handle) return String;
   --  Return the external name of a tty. The name depends on the tty handling
   --  on the given target. It will typically look like: "/dev/ptya1"

   function TTY_Descriptor
     (Handle : TTY_Handle) return GNAT.OS_Lib.File_Descriptor;
   --  Return the low level descriptor associated with Handle

private

   type TTY_Handle is record
      Handle : System.Address := System.Null_Address;
   end record;

end GNAT.TTY;
