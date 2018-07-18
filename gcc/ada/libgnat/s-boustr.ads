------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                S Y S T E M . B O U N D E D _ S T R I N G S               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                       Copyright (C) 2016-2018, AdaCore                   --
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

--  A very simple implentation of bounded strings, used by tracebacks

package System.Bounded_Strings is
   type Bounded_String (Max_Length : Natural) is limited private;
   --  A string whose length is bounded by Max_Length. The bounded string is
   --  empty at initialization.

   procedure Append (X : in out Bounded_String; C : Character);
   procedure Append (X : in out Bounded_String; S : String);
   --  Append a character or a string to X. If the bounded string is full,
   --  extra characters are simply dropped.

   function To_String (X : Bounded_String) return String;
   function "+" (X : Bounded_String) return String renames To_String;
   --  Convert to a normal string

   procedure Append_Address (X : in out Bounded_String; A : Address);
   --  Append an address to X

   function Is_Full (X : Bounded_String) return Boolean;
   --  Return True iff X is full and any character or string will be dropped
   --  if appended.
private
   type Bounded_String (Max_Length : Natural) is limited record
      Length : Natural := 0;
      --  Current length of the string

      Chars  : String (1 .. Max_Length);
      --  String content
   end record;
end System.Bounded_Strings;
