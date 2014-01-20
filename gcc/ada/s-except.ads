------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                    S Y S T E M . E X C E P T I O N S                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2006-2013, Free Software Foundation, Inc.         --
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

pragma Compiler_Unit;

package System.Exceptions is

   pragma Preelaborate_05;
   --  To let Ada.Exceptions "with" us and let us "with" Standard_Library

   ZCX_By_Default : constant Boolean;
   --  Visible copy to allow Ada.Exceptions to know the exception model

private

   type Require_Body;
   --  Dummy Taft-amendment type to make it legal (and required) to provide
   --  a body for this package.
   --
   --  We do this because this unit used to have a body in earlier versions
   --  of GNAT, and it causes various bootstrap path problems etc if we remove
   --  a body, since we may pick up old unwanted bodies.
   --
   --  Note: we use this standard Ada method of requiring a body rather
   --  than the cleaner pragma No_Body because System.Exceptions is a compiler
   --  unit, and older bootstrap compilers do not support pragma No_Body. This
   --  type can be removed, and s-except.adb can be replaced by a source
   --  containing just that pragma, when we decide to move to a 2008 compiler
   --  as the minimal bootstrap compiler version. ???

   ZCX_By_Default : constant Boolean := System.ZCX_By_Default;

   Foreign_Exception : exception;
   pragma Unreferenced (Foreign_Exception);
   --  This hidden exception is used to represent non-Ada exception to
   --  Ada handlers. It is in fact referenced by its linking name.

end System.Exceptions;
