------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                             G N A T . M D 5                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2002-2005, AdaCore                     --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This package implements the MD5 Message-Digest Algorithm as described in
--  RFC 1321. The complete text of RFC 1321 can be found at:
--
--          http://www.ietf.org/rfc/rfc1321.txt
--
--  The implementation is derived from the RSA Data Secutity, Inc. MD5
--  Message-Digest Algorithm, as described in RFC 1321.

with Ada.Streams;
with Interfaces;

package GNAT.MD5 is

   type Context is private;
   --  This type is the four-word (16 byte) MD buffer, as described in
   --  RFC 1321 (3.3). It initial value is Initial_Context below.

   Initial_Context : constant Context;
   --  Initial value of a Context object. May be used to reinitialize
   --  a Context value by simple assignment of this value to the object.

   procedure Update
     (C     : in out Context;
      Input : String);
   procedure Wide_Update
     (C     : in out Context;
      Input : Wide_String);
   procedure Update
     (C     : in out Context;
      Input : Ada.Streams.Stream_Element_Array);
   --  Modify the Context C. If C has the initial value Initial_Context,
   --  then, after a call to one of these procedures, Digest (C) will return
   --  the Message-Digest of Input.
   --
   --  These procedures may be called successively with the same context and
   --  different inputs, and these several successive calls will produce
   --  the same final context as a call with the concatenation of the inputs.

   subtype Message_Digest is String (1 .. 32);
   --  The string type returned by function Digest

   function Digest (C : Context) return Message_Digest;
   --  Extracts the Message-Digest from a context. This function should be
   --  used after one or several calls to Update.

   function Digest      (S : String)      return Message_Digest;
   function Wide_Digest (W : Wide_String) return Message_Digest;
   function Digest
     (A    : Ada.Streams.Stream_Element_Array)
      return Message_Digest;
   --  These functions are equivalent to the corresponding Update (or
   --  Wide_Update) on a default initialized Context, followed by Digest
   --  on the resulting Context.

private

   --  Magic numbers

   Initial_A : constant := 16#67452301#;
   Initial_B : constant := 16#EFCDAB89#;
   Initial_C : constant := 16#98BADCFE#;
   Initial_D : constant := 16#10325476#;

   type Context is record
      A : Interfaces.Unsigned_32 := Initial_A;
      B : Interfaces.Unsigned_32 := Initial_B;
      C : Interfaces.Unsigned_32 := Initial_C;
      D : Interfaces.Unsigned_32 := Initial_D;
      Buffer : String (1 .. 64)  := (others => ASCII.NUL);
      Last   : Natural := 0;
      Length : Natural := 0;
   end record;

   Initial_Context : constant Context :=
     (A => Initial_A, B => Initial_B, C => Initial_C, D => Initial_D,
      Buffer => (others => ASCII.NUL), Last => 0, Length => 0);

end GNAT.MD5;
