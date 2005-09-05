------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                         I N T E R F A C E S . C                          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
-- This specification is adapted from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

with System.Parameters;

package Interfaces.C is
   pragma Pure;

   --  Declaration's based on C's <limits.h>

   CHAR_BIT  : constant := 8;
   SCHAR_MIN : constant := -128;
   SCHAR_MAX : constant := 127;
   UCHAR_MAX : constant := 255;

   --  Signed and Unsigned Integers. Note that in GNAT, we have ensured that
   --  the standard predefined Ada types correspond to the standard C types

   --  Note: the Integer qualifications used in the declaration of type long
   --  avoid ambiguities when compiling in the presence of s-auxdec.ads and
   --  a non-private system.address type.

   type int   is new Integer;
   type short is new Short_Integer;
   type long  is range -(2 ** (System.Parameters.long_bits - Integer'(1)))
     .. +(2 ** (System.Parameters.long_bits - Integer'(1))) - 1;

   type signed_char is range SCHAR_MIN .. SCHAR_MAX;
   for signed_char'Size use CHAR_BIT;

   type unsigned       is mod 2 ** int'Size;
   type unsigned_short is mod 2 ** short'Size;
   type unsigned_long  is mod 2 ** long'Size;

   type unsigned_char is mod (UCHAR_MAX + 1);
   for unsigned_char'Size use CHAR_BIT;

   subtype plain_char is unsigned_char; -- ??? should be parametrized

   --  Note: the Integer qualifications used in the declaration of ptrdiff_t
   --  avoid ambiguities when compiling in the presence of s-auxdec.ads and
   --  a non-private system.address type.

   type ptrdiff_t is
     range -(2 ** (Standard'Address_Size - Integer'(1))) ..
           +(2 ** (Standard'Address_Size - Integer'(1)) - 1);

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

   function To_C   (Item : Character) return char;
   function To_Ada (Item : char)      return Character;

   type char_array is array (size_t range <>) of aliased char;
   for char_array'Component_Size use CHAR_BIT;

   function Is_Nul_Terminated (Item : in char_array) return Boolean;

   function To_C
     (Item       : String;
      Append_Nul : Boolean := True) return char_array;

   function To_Ada
     (Item     : char_array;
      Trim_Nul : Boolean := True) return String;

   procedure To_C
     (Item       : String;
      Target     : out char_array;
      Count      : out size_t;
      Append_Nul : Boolean := True);

   procedure To_Ada
     (Item     : char_array;
      Target   : out String;
      Count    : out Natural;
      Trim_Nul : Boolean := True);

   ------------------------------------
   -- Wide Character and Wide String --
   ------------------------------------

   type wchar_t is new Wide_Character;
   for wchar_t'Size use Standard'Wchar_T_Size;

   wide_nul : constant wchar_t := wchar_t'First;

   function To_C   (Item : Wide_Character) return wchar_t;
   function To_Ada (Item : wchar_t)        return Wide_Character;

   type wchar_array is array (size_t range <>) of aliased wchar_t;

   function Is_Nul_Terminated (Item : wchar_array) return Boolean;

   function To_C
     (Item       : Wide_String;
      Append_Nul : Boolean := True) return wchar_array;

   function To_Ada
     (Item     : wchar_array;
      Trim_Nul : Boolean := True) return Wide_String;

   procedure To_C
     (Item       : Wide_String;
      Target     : out wchar_array;
      Count      : out size_t;
      Append_Nul : Boolean := True);

   procedure To_Ada
     (Item     : wchar_array;
      Target   : out Wide_String;
      Count    : out Natural;
      Trim_Nul : Boolean := True);

   Terminator_Error : exception;

   --  The remaining declarations are for Ada 2005 (AI-285)

   --  ISO/IEC 10646:2003 compatible types defined by SC22/WG14 document N1010

   type char16_t is new Wide_Character;
   pragma Ada_05 (char16_t);

   char16_nul : constant char16_t := char16_t'Val (0);
   pragma Ada_05 (char16_nul);

   function To_C (Item : Wide_Character) return char16_t;
   pragma Ada_05 (To_C);

   function To_Ada (Item : char16_t) return Wide_Character;
   pragma Ada_05 (To_Ada);

   type char16_array is array (size_t range <>) of aliased char16_t;
   pragma Ada_05 (char16_array);

   function Is_Nul_Terminated (Item : char16_array) return Boolean;
   pragma Ada_05 (Is_Nul_Terminated);

   function To_C
     (Item       : Wide_String;
      Append_Nul : Boolean := True) return char16_array;
   pragma Ada_05 (To_C);

   function To_Ada
     (Item     : char16_array;
      Trim_Nul : Boolean := True) return Wide_String;
   pragma Ada_05 (To_Ada);

   procedure To_C
     (Item       : Wide_String;
      Target     : out char16_array;
      Count      : out size_t;
      Append_Nul : Boolean := True);
   pragma Ada_05 (To_C);

   procedure To_Ada
     (Item     : char16_array;
      Target   : out Wide_String;
      Count    : out Natural;
      Trim_Nul : Boolean := True);
   pragma Ada_05 (To_Ada);

   type char32_t is new Wide_Wide_Character;
   pragma Ada_05 (char32_t);

   char32_nul : constant char32_t := char32_t'Val (0);
   pragma Ada_05 (char32_nul);

   function To_C (Item : Wide_Wide_Character) return char32_t;
   pragma Ada_05 (To_C);

   function To_Ada (Item : char32_t) return Wide_Wide_Character;
   pragma Ada_05 (To_Ada);

   type char32_array is array (size_t range <>) of aliased char32_t;
   pragma Ada_05 (char32_array);

   function Is_Nul_Terminated (Item : char32_array) return Boolean;
   pragma Ada_05 (Is_Nul_Terminated);

   function To_C
     (Item       : Wide_Wide_String;
      Append_Nul : Boolean := True) return char32_array;
   pragma Ada_05 (To_C);

   function To_Ada
     (Item     : char32_array;
      Trim_Nul : Boolean := True) return Wide_Wide_String;
   pragma Ada_05 (To_Ada);

   procedure To_C
     (Item       : Wide_Wide_String;
      Target     : out char32_array;
      Count      : out size_t;
      Append_Nul : Boolean := True);
   pragma Ada_05 (To_C);

   procedure To_Ada
     (Item     : char32_array;
      Target   : out Wide_Wide_String;
      Count    : out Natural;
      Trim_Nul : Boolean := True);
   pragma Ada_05 (To_Ada);

end Interfaces.C;
