------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                            G N A T . S H A 1                             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2002-2006, AdaCore                     --
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

--  This package implements the US Secure Hash Algorithm 1 (SHA1) as described
--  in RFC 3174. The complete text of RFC 3174 can be found at:

--          http://www.ietf.org/rfc/rfc3174.txt

--  Note: the code for this unit is derived from GNAT.MD5

with Ada.Streams;
with Interfaces;

package GNAT.SHA1 is

   type Context is private;
   --  This type holds the five-word (20 byte) buffer H, as described in
   --  RFC 3174 (6.1). Its initial value is Initial_Context below.

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

   subtype Message_Digest is String (1 .. 40);
   --  The string type returned by function Digest

   function Digest (C : Context) return Message_Digest;
   --  Extracts the Message-Digest from a context. This function should be
   --  used after one or several calls to Update.

   function Digest      (S : String)      return Message_Digest;
   function Wide_Digest (W : Wide_String) return Message_Digest;
   function Digest
     (A : Ada.Streams.Stream_Element_Array) return Message_Digest;
   --  These functions are equivalent to the corresponding Update (or
   --  Wide_Update) on a default initialized Context, followed by Digest
   --  on the resulting Context.

private

   --  Magic numbers

   Initial_H0 : constant := 16#67452301#;
   Initial_H1 : constant := 16#EFCDAB89#;
   Initial_H2 : constant := 16#98BADCFE#;
   Initial_H3 : constant := 16#10325476#;
   Initial_H4 : constant := 16#C3D2E1F0#;

   type H_Type is array (0 .. 4) of Interfaces.Unsigned_32;

   Initial_H : constant H_Type :=
                (0 => Initial_H0,
                 1 => Initial_H1,
                 2 => Initial_H2,
                 3 => Initial_H3,
                 4 => Initial_H4);

   type Context is record
      H      : H_Type := Initial_H;
      Buffer : String (1 .. 64)  := (others => ASCII.NUL);
      Last   : Natural := 0;
      Length : Natural := 0;
   end record;

   Initial_Context : constant Context :=
     (H => Initial_H,
      Buffer => (others => ASCII.NUL), Last => 0, Length => 0);

end GNAT.SHA1;
