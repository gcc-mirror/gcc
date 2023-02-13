------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                      G N A T . S T R I N G _ H A S H                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--             Copyright (C) 2015-2023, Free Software Foundation, Inc.      --
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

--  This package provides a generic hashing function over strings, suitable for
--  use with a string keyed hash table. In particular, it is the basis for the
--  string hash functions in Ada.Containers.
--
--  The algorithm used here is not appropriate for applications that require
--  cryptographically strong hashes, or for applications that wish to use very
--  wide hash values as pseudo unique identifiers. In such cases please refer
--  to GNAT.SHA1 and GNAT.MD5.

with System.String_Hash;

package GNAT.String_Hash renames System.String_Hash;
