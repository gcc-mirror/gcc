-----------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                         I N T E R F A C E S . C                          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
-- This specification is adapted from the Ada Reference Manual for use with --
-- GNAT  Hi Integrity Edition.  In accordance  with the  copyright of  that --
-- document,  you can  freely copy and modify this specification,  provided --
-- that if you  redistribute a modified version,  any changes that you have --
-- made are clearly indicated.                                              --
--                                                                          --
------------------------------------------------------------------------------

--  This version contains only the type definitions for standard interfacing
--  with C. All functions have been removed from the original spec.

package Interfaces.C is
pragma Pure (C);

   --  Declaration's based on C's <limits.h>

   CHAR_BIT  : constant := 8;
   SCHAR_MIN : constant := -128;
   SCHAR_MAX : constant := 127;
   UCHAR_MAX : constant := 255;

   --  Signed and Unsigned Integers. Note that in GNAT, we have ensured that
   --  the standard predefined Ada types correspond to the standard C types

   type int   is new Integer;
   type short is new Short_Integer;
   type long  is new Long_Integer;

   type signed_char is range SCHAR_MIN .. SCHAR_MAX;
   for signed_char'Size use CHAR_BIT;

   type unsigned       is mod 2 ** int'Size;
   type unsigned_short is mod 2 ** short'Size;
   type unsigned_long  is mod 2 ** long'Size;

   type unsigned_char is mod (UCHAR_MAX + 1);
   for unsigned_char'Size use CHAR_BIT;

   subtype plain_char is unsigned_char;

   type ptrdiff_t is
     range -(2 ** (Standard'Address_Size - 1)) ..
           +(2 ** (Standard'Address_Size - 1) - 1);

   type size_t is mod 2 ** Standard'Address_Size;

   --  Floating-Point

   type C_float     is new Float;
   type double      is new Standard.Long_Float;
   type long_double is new Standard.Long_Long_Float;

   ----------------------------
   -- Characters and Strings --
   ----------------------------

   type char is new Character;

   nul : constant char := char'First;

   type char_array is array (size_t range <>) of aliased char;
   for char_array'Component_Size use CHAR_BIT;

   ------------------------------------
   -- Wide Character and Wide String --
   ------------------------------------

   type wchar_t is new Wide_Character;
   for wchar_t'Size use Standard'Wchar_T_Size;

   wide_nul : constant wchar_t := wchar_t'First;

   type wchar_array is array (size_t range <>) of aliased wchar_t;

end Interfaces.C;
