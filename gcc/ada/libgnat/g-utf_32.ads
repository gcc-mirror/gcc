------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                          G N A T . U T F _ 3 2                           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2005-2017, Free Software Foundation, Inc.         --
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

--  This package is an internal package that provides basic character
--  classification capabilities needed by the compiler for handling full
--  32-bit wide wide characters. We avoid the use of the actual type
--  Wide_Wide_Character, since we want to use these routines in the compiler
--  itself, and we want to be able to compile the compiler with old versions
--  of GNAT that did not implement Wide_Wide_Character.

--  This package is available directly for use in application programs,
--  and also serves as the basis for Ada.Wide_Wide_Characters.Unicode and
--  Ada.Wide_Characters.Unicode, which can also be used directly.

--  See file s-utf_32.ads for full documentation of the interface

with System.UTF_32;

package GNAT.UTF_32 renames System.UTF_32;
