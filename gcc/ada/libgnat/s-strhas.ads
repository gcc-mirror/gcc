------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                    S Y S T E M . S T R I N G _ H A S H                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2009-2023, Free Software Foundation, Inc.         --
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
--  cryptographically strong hashes, or for application which wish to use very
--  wide hash values as pseudo unique identifiers. In such cases please refer
--  to GNAT.SHA1 and GNAT.MD5.
--
--  Note: this package is in the System hierarchy so that it can be directly
--  be used by other predefined packages. User access to this package is via
--  a renaming of this package in GNAT.String_Hash (file g-strhas.ads).

package System.String_Hash is
   pragma Pure;

   generic
      type Char_Type is (<>);
      --  The character type composing the key string type

      type Key_Type is array (Positive range <>) of Char_Type;
      --  The string type to use as a hash key

      type Hash_Type is mod <>;
      --  The type to be returned as a hash value. This must be a 32-bit
      --  unsigned type with full range 0 .. 2**32-1, no other type is allowed
      --  for this instantiation (checked in the body by Compile_Time_Error).

   function Hash (Key : Key_Type) return Hash_Type;
   pragma Inline (Hash);
   --  Compute a hash value for a key

end System.String_Hash;
