------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               R I D E N T                                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2021, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This package defines the set of restriction identifiers for use by the
--  compiler and binder. It is in a separate package from Restrict so that
--  it can be used by the binder without dragging in unneeded compiler
--  packages.

--  Note: the actual definitions of the types are in package System.Rident,
--  and this package is merely an instantiation of that package. The point
--  of this level of generic indirection is to allow the compile time use
--  to have the image tables available (this package is not compiled with
--  Discard_Names), while at run-time we do not want those image tables.

--  Rather than have clients instantiate System.Rident directly, we have the
--  single instantiation here at the library level, which means that we only
--  have one copy of the image tables.

with System.Rident;

package Rident is new System.Rident;
