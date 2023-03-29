------------------------------------------------------------------------------
--                                                                          --
--                          GNAT RUN-TIME COMPONENTS                        --
--                                                                          --
--            A D A . S T R I N G S . U N B O U N D E D . A U X             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2023, Free Software Foundation, Inc.         --
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

--  This child package of Ada.Strings.Unbounded provides some specialized
--  access functions which are intended to allow more efficient use of the
--  facilities of Ada.Strings.Unbounded, particularly by other layered
--  utilities (such as GNAT.SPITBOL.Patterns).

package Ada.Strings.Unbounded.Aux is
   pragma Preelaborate;

   subtype Big_String is String (1 .. Positive'Last);
   pragma Suppress_Initialization (Big_String);
   --  Type used to obtain string access to given address. Initialization is
   --  suppressed, since we never want to have variables of this type, and
   --  we never want to attempt initialiazation of virtual variables of this
   --  type (e.g. when pragma Normalize_Scalars is used).

   type Big_String_Access is access all Big_String;
   for Big_String_Access'Storage_Size use 0;
   --  We use this access type to pass a pointer to an area of storage to be
   --  accessed as a string. Of course when this pointer is used, it is the
   --  responsibility of the accessor to ensure proper bounds. The storage
   --  size clause ensures we do not allocate variables of this type.

   procedure Get_String
     (U : Unbounded_String;
      S : out Big_String_Access;
      L : out Natural);
   pragma Inline (Get_String);
   --  Return the internal string pointer used in the representation of an
   --  unbounded string as well as the actual current length (which may be less
   --  than S.all'Length because in general there can be extra space assigned).
   --  The characters of this string may be not be modified via the returned
   --  pointer, and are valid only as long as the original unbounded string is
   --  not accessed or modified.
   --
   --  This procedure is much more efficient than the use of To_String
   --  since it avoids the need to copy the string. The lower bound of the
   --  referenced string returned by this call is always one, so the actual
   --  string data is always accessible as S (1 .. L).

   procedure Set_String
     (U      : out Unbounded_String;
      Length : Positive;
      Set    : not null access procedure (S : out String));
   pragma Inline (Set_String);
   --  Create an unbounded string U with the given Length, using Set to fill
   --  the contents of U.

end Ada.Strings.Unbounded.Aux;
