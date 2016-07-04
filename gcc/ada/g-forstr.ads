------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                G N A T . F O R M A T T E D _ S T R I N G                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--           Copyright (C) 2014-2016, Free Software Foundation, Inc.        --
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

--  This package add support for formatted string as supported by C printf()

--  A simple usage is:
--
--     Put_Line (-(+"%s" & "a string"));
--
--  or with a constant for the format:
--
--     declare
--       Format : constant Formatted_String := +"%s";
--     begin
--       Put_Line (-(Format & "a string"));
--     end;
--
--  Finally a more complex example:
--
--     declare
--        F : Formatted_String := +"['%c' ; %10d]";
--        C : Character := 'v';
--        I : Integer := 98;
--     begin
--        F := F & C & I;
--        Put_Line (-F);
--     end;

--  Which will display:

--     ['v' ;         98]

--  Each format specifier is: %[flags][width][.precision][length]specifier

--  Specifiers:
--    d or i    Signed decimal integer
--    u         Unsigned decimal integer
--    o         Unsigned octal
--    x         Unsigned hexadecimal integer
--    X         Unsigned hexadecimal integer (uppercase)
--    f         Decimal floating point, lowercase
--    F         Decimal floating point, uppercase
--    e         Scientific notation (mantissa/exponent), lowercase
--    E         Scientific notation (mantissa/exponent), uppercase
--    g         Use the shortest representation: %e or %f
--    G         Use the shortest representation: %E or %F
--    c         Character
--    s         String of characters
--    p         Pointer address
--    %         A % followed by another % character will write a single %

--  Flags:

--    -         Left-justify within the given field width;
--              Right justification is the default.

--    +         Forces to preceed the result with a plus or minus sign (+ or -)
--              even for positive numbers. By default, only negative numbers
--              are preceded with a - sign.

--    (space)   If no sign is going to be written, a blank space is inserted
--              before the value.

--    #         Used with o, x or X specifiers the value is preceeded with
--              0, 0x or 0X respectively for values different than zero.
--              Used with a, A, e, E, f, F, g or G it forces the written
--              output to contain a decimal point even if no more digits
--              follow. By default, if no digits follow, no decimal point is
--              written.

--    ~         As above, but using Ada style based <base>#<number>#

--    0         Left-pads the number with zeroes (0) instead of spaces when
--              padding is specified.

--  Width:
--    number    Minimum number of characters to be printed. If the value to
--              be printed is shorter than this number, the result is padded
--              with blank spaces. The value is not truncated even if the
--              result is larger.

--    *         The width is not specified in the format string, but as an
--              additional integer value argument preceding the argument that
--              has to be formatted.
--  Precision:
--    number    For integer specifiers (d, i, o, u, x, X): precision specifies
--              the minimum number of digits to be written. If the value to be
--              written is shorter than this number, the result is padded with
--              leading zeros. The value is not truncated even if the result
--              is longer. A precision of 0 means that no character is written
--              for the value 0.

--              For e, E, f and F specifiers: this is the number of digits to
--              be printed after the decimal point (by default, this is 6).
--              For g and G specifiers: This is the maximum number of
--              significant digits to be printed.

--              For s: this is the maximum number of characters to be printed.
--              By default all characters are printed until the ending null
--              character is encountered.

--              If the period is specified without an explicit value for
--              precision, 0 is assumed.

--    .*        The precision is not specified in the format string, but as an
--              additional integer value argument preceding the argument that
--              has to be formatted.

with Ada.Text_IO;
with System;

private with Ada.Finalization;
private with Ada.Strings.Unbounded;

package GNAT.Formatted_String is
   use Ada;

   type Formatted_String (<>) is private;
   --  A format string as defined for printf routine

   Format_Error : exception;
   --  Raised for every mismatch between the parameter and the expected format
   --  and for malformed format.

   function "+" (Format : String) return Formatted_String;
   --  Create the format string

   function "-" (Format : Formatted_String) return String;
   --  Get the result of the formatted string corresponding to the current
   --  rendering (up to the last parameter formated).

   function "&"
     (Format : Formatted_String;
      Var    : Character) return Formatted_String;
   --  A character, expect a %c

   function "&"
     (Format : Formatted_String;
      Var    : String) return Formatted_String;
   --  A string, expect a %s

   function "&"
     (Format : Formatted_String;
      Var    : Boolean) return Formatted_String;
   --  A boolean image, expect a %s

   function "&"
     (Format : Formatted_String;
      Var    : Integer) return Formatted_String;
   --  An integer, expect a %d, %o, %x, %X

   function "&"
     (Format : Formatted_String;
      Var    : Long_Integer) return Formatted_String;
   --  As above

   function "&"
     (Format : Formatted_String;
      Var    : System.Address) return Formatted_String;
   --  An address, expect a %p

   function "&"
     (Format : Formatted_String;
      Var    : Float) return Formatted_String;
   --  A float, expect %f, %e, %F, %E, %g, %G

   function "&"
     (Format : Formatted_String;
      Var    : Long_Float) return Formatted_String;
   --  As above

   function "&"
     (Format : Formatted_String;
      Var    : Duration) return Formatted_String;
   --  As above

   --  Some generics

   generic
      type Int is range <>;

      with procedure Put
        (To   : out String;
         Item : Int;
         Base : Text_IO.Number_Base);
   function Int_Format
     (Format : Formatted_String;
      Var    : Int) return Formatted_String;
   --  As for Integer above

   generic
      type Int is mod <>;

      with procedure Put
        (To   : out String;
         Item : Int;
         Base : Text_IO.Number_Base);
   function Mod_Format
     (Format : Formatted_String;
      Var    : Int) return Formatted_String;
   --  As for Integer above

   generic
      type Flt is digits <>;

      with procedure Put
        (To   : out String;
         Item : Flt;
         Aft  : Text_IO.Field;
         Exp  : Text_IO.Field);
   function Flt_Format
     (Format : Formatted_String;
      Var    : Flt) return Formatted_String;
   --  As for Float above

   generic
      type Flt is delta <>;

      with procedure Put
        (To   : out String;
         Item : Flt;
         Aft  : Text_IO.Field;
         Exp  : Text_IO.Field);
   function Fixed_Format
     (Format : Formatted_String;
      Var    : Flt) return Formatted_String;
   --  As for Float above

   generic
      type Flt is delta <> digits <>;

      with procedure Put
        (To   : out String;
         Item : Flt;
         Aft  : Text_IO.Field;
         Exp  : Text_IO.Field);
   function Decimal_Format
     (Format : Formatted_String;
      Var    : Flt) return Formatted_String;
   --  As for Float above

   generic
      type Enum is (<>);
   function Enum_Format
     (Format : Formatted_String;
      Var    : Enum) return Formatted_String;
   --  As for String above, output the string representation of the enumeration

private
   use Ada.Strings.Unbounded;

   type I_Vars is array (Positive range 1 .. 2) of Integer;
   --  Used to keep 2 numbers for the possible * for the width and precision

   type Data (Size : Natural) is record
      Ref_Count    : Natural := 1;
      Format       : String (1 .. Size); -- the format string
      Index        : Positive := 1;      -- format index for next value
      Result       : Unbounded_String;   -- current value
      Current      : Natural;            -- the current format number
      Stored_Value : Natural := 0;       -- number of stored values in Stack
      Stack        : I_Vars;
   end record;

   type Data_Access is access Data;

   --  The formatted string record is controlled and do not need an initialize
   --  as it requires an explit initial value. This is given with "+" and
   --  properly initialize the record at this point.

   type Formatted_String is new Finalization.Controlled with record
      D : Data_Access;
   end record;

   overriding procedure Adjust   (F : in out Formatted_String);
   overriding procedure Finalize (F : in out Formatted_String);

end GNAT.Formatted_String;
