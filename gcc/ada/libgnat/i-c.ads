------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                         I N T E R F A C E S . C                          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

--  Preconditions in this unit are meant for analysis only, not for run-time
--  checking, so that the expected exceptions are raised. This is enforced by
--  setting the corresponding assertion policy to Ignore. Postconditions and
--  contract cases should not be executed at runtime as well, in order not to
--  slow down the execution of these functions.

pragma Assertion_Policy (Pre            => Ignore,
                         Post           => Ignore,
                         Contract_Cases => Ignore,
                         Ghost          => Ignore);

with System.Parameters;

package Interfaces.C
  with SPARK_Mode, Pure
is
   pragma Annotate (GNATprove, Always_Return, C);

   --  Each of the types declared in Interfaces.C is C-compatible.

   --  The types int, short, long, unsigned, ptrdiff_t, size_t, double,
   --  char, wchar_t, char16_t, and char32_t correspond respectively to the
   --  C types having the same names. The types signed_char, unsigned_short,
   --  unsigned_long, unsigned_char, C_bool, C_float, and long_double
   --  correspond respectively to the C types signed char, unsigned
   --  short, unsigned long, unsigned char, bool, float, and long double.

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
   type long_long is new Long_Long_Integer;

   type signed_char is range SCHAR_MIN .. SCHAR_MAX;
   for signed_char'Size use CHAR_BIT;

   type unsigned           is mod 2 ** int'Size;
   type unsigned_short     is mod 2 ** short'Size;
   type unsigned_long      is mod 2 ** long'Size;
   type unsigned_long_long is mod 2 ** long_long'Size;

   type unsigned_char is mod (UCHAR_MAX + 1);
   for unsigned_char'Size use CHAR_BIT;

   --  Note: Ada RM states that the type of the subtype plain_char is either
   --  signed_char or unsigned_char, depending on the C implementation. GNAT
   --  instead choses unsigned_char always.

   subtype plain_char is unsigned_char;

   --  Note: the Integer qualifications used in the declaration of ptrdiff_t
   --  avoid ambiguities when compiling in the presence of s-auxdec.ads and
   --  a non-private system.address type.

   type ptrdiff_t is
     range -(2 ** (System.Parameters.ptr_bits - Integer'(1))) ..
           +(2 ** (System.Parameters.ptr_bits - Integer'(1)) - 1);

   type size_t is mod 2 ** System.Parameters.ptr_bits;

   --  Boolean type

   type C_bool is new Boolean;
   pragma Convention (C, C_bool);

   --  Floating-Point

   type C_float     is new Float;
   type double      is new Standard.Long_Float;
   type long_double is new Standard.Long_Long_Float;

   ----------------------------
   -- Characters and Strings --
   ----------------------------

   type char is new Character;

   nul : constant char := char'First;

   --  The functions To_C and To_Ada map between the Ada type Character and the
   --  C type char.

   function To_C (Item : Character) return char
   with
     Post => To_C'Result = char'Val (Character'Pos (Item));

   function To_Ada (Item : char) return Character
   with
     Post => To_Ada'Result = Character'Val (char'Pos (Item));

   type char_array is array (size_t range <>) of aliased char;
   for char_array'Component_Size use CHAR_BIT;

   function Is_Nul_Terminated (Item : char_array) return Boolean
   with
     Post => Is_Nul_Terminated'Result = (for some C of Item => C = nul);
   --  The result of Is_Nul_Terminated is True if Item contains nul, and is
   --  False otherwise.

   function C_Length_Ghost (Item : char_array) return size_t
   with
     Ghost,
     Pre  => Is_Nul_Terminated (Item),
     Post => C_Length_Ghost'Result <= Item'Last - Item'First
       and then Item (Item'First + C_Length_Ghost'Result) = nul
       and then (for all J in Item'First .. Item'First + C_Length_Ghost'Result
                   when J /= Item'First + C_Length_Ghost'Result =>
                     Item (J) /= nul);
   --  Ghost function to compute the length of a char_array up to the first nul
   --  character.

   function To_C
     (Item       : String;
      Append_Nul : Boolean := True) return char_array
   with
     Pre  => not (Append_Nul = False and then Item'Length = 0),
     Post => To_C'Result'First = 0
       and then To_C'Result'Length =
         (if Append_Nul then Item'Length + 1 else Item'Length)
       and then (for all J in Item'Range =>
                   To_C'Result (size_t (J - Item'First)) = To_C (Item (J)))
       and then (if Append_Nul then To_C'Result (To_C'Result'Last) = nul);
   --  The result of To_C is a char_array value of length Item'Length (if
   --  Append_Nul is False) or Item'Length+1 (if Append_Nul is True). The lower
   --  bound is 0. For each component Item(I), the corresponding component
   --  in the result is To_C applied to Item(I). The value nul is appended if
   --  Append_Nul is True. If Append_Nul is False and Item'Length is 0, then
   --  To_C propagates Constraint_Error.

   function To_Ada
     (Item     : char_array;
      Trim_Nul : Boolean := True) return String
   with
     Pre  => (if Trim_Nul then
                Is_Nul_Terminated (Item)
                  and then C_Length_Ghost (Item) <= size_t (Natural'Last)
              else
                Item'Last - Item'First < size_t (Natural'Last)),
     Post => To_Ada'Result'First = 1
       and then To_Ada'Result'Length =
         (if Trim_Nul then C_Length_Ghost (Item) else Item'Length)
       and then (for all J in To_Ada'Result'Range =>
                   To_Ada'Result (J) =
                     To_Ada (Item (size_t (J) - 1 + Item'First)));
   --  The result of To_Ada is a String whose length is Item'Length (if
   --  Trim_Nul is False) or the length of the slice of Item preceding the
   --  first nul (if Trim_Nul is True). The lower bound of the result is 1.
   --  If Trim_Nul is False, then for each component Item(I) the corresponding
   --  component in the result is To_Ada applied to Item(I). If Trim_Nul
   --  is True, then for each component Item(I) before the first nul the
   --  corresponding component in the result is To_Ada applied to Item(I). The
   --  function propagates Terminator_Error if Trim_Nul is True and Item does
   --  not contain nul.

   procedure To_C
     (Item       : String;
      Target     : out char_array;
      Count      : out size_t;
      Append_Nul : Boolean := True)
   with
     Relaxed_Initialization => Target,
     Pre  => Target'Length >=
       (if Append_Nul then Item'Length + 1 else Item'Length),
     Post => Count = (if Append_Nul then Item'Length + 1 else Item'Length)
       and then
         (if Count /= 0 then
           Target (Target'First .. Target'First + (Count - 1))'Initialized)
       and then
         (for all J in Item'Range =>
           Target (Target'First + size_t (J - Item'First)) = To_C (Item (J)))
       and then
         (if Append_Nul then Target (Target'First + (Count - 1)) = nul);
   --  For procedure To_C, each element of Item is converted (via the To_C
   --  function) to a char, which is assigned to the corresponding element of
   --  Target. If Append_Nul is True, nul is then assigned to the next element
   --  of Target. In either case, Count is set to the number of Target elements
   --  assigned. If Target is not long enough, Constraint_Error is propagated.

   procedure To_Ada
     (Item     : char_array;
      Target   : out String;
      Count    : out Natural;
      Trim_Nul : Boolean := True)
   with
     Relaxed_Initialization => Target,
     Pre  => (if Trim_Nul then
                Is_Nul_Terminated (Item)
                  and then C_Length_Ghost (Item) <= size_t (Target'Length)
              else
                Item'Last - Item'First < size_t (Target'Length)),
     Post => Count =
         (if Trim_Nul then Natural (C_Length_Ghost (Item)) else Item'Length)
       and then
         (if Count /= 0 then
           Target (Target'First .. Target'First + (Count - 1))'Initialized)
       and then
         (for all J in Target'First .. Target'First + (Count - 1) =>
           Target (J) =
             To_Ada (Item (size_t (J - Target'First) + Item'First)));
   --  For procedure To_Ada, each element of Item (if Trim_Nul is False) or
   --  each element of Item preceding the first nul (if Trim_Nul is True) is
   --  converted (via the To_Ada function) to a Character, which is assigned
   --  to the corresponding element of Target. Count is set to the number of
   --  Target elements assigned. If Target is not long enough, Constraint_Error
   --  is propagated. If Trim_Nul is True and Item does not contain nul, then
   --  Terminator_Error is propagated.

   ------------------------------------
   -- Wide Character and Wide String --
   ------------------------------------

   type wchar_t is new Wide_Character;
   for wchar_t'Size use Standard'Wchar_T_Size;

   wide_nul : constant wchar_t := wchar_t'First;

   --  To_C and To_Ada provide the mappings between the Ada and C wide
   --  character types.

   function To_C (Item : Wide_Character) return wchar_t
   with
     Post => To_C'Result = wchar_t (Item);

   function To_Ada (Item : wchar_t) return Wide_Character
   with
     Post => To_Ada'Result = Wide_Character (Item);

   type wchar_array is array (size_t range <>) of aliased wchar_t;

   function Is_Nul_Terminated (Item : wchar_array) return Boolean
   with
     Post => Is_Nul_Terminated'Result = (for some C of Item => C = wide_nul);
   --  The result of Is_Nul_Terminated is True if Item contains wide_nul, and
   --  is False otherwise.

   --  The To_C and To_Ada subprograms that convert between Wide_String and
   --  wchar_array have analogous effects to the To_C and To_Ada subprograms
   --  that convert between String and char_array, except that wide_nul is
   --  used instead of nul.

   function C_Length_Ghost (Item : wchar_array) return size_t
   with
     Ghost,
     Pre  => Is_Nul_Terminated (Item),
     Post => C_Length_Ghost'Result <= Item'Last - Item'First
       and then Item (Item'First + C_Length_Ghost'Result) = wide_nul
       and then (for all J in Item'First .. Item'First + C_Length_Ghost'Result
                   when J /= Item'First + C_Length_Ghost'Result =>
                     Item (J) /= wide_nul);
   --  Ghost function to compute the length of a wchar_array up to the first
   --  wide_nul character.

   function To_C
     (Item       : Wide_String;
      Append_Nul : Boolean := True) return wchar_array
   with
     Pre  => not (Append_Nul = False and then Item'Length = 0),
     Post => To_C'Result'First = 0
       and then To_C'Result'Length =
         (if Append_Nul then Item'Length + 1 else Item'Length)
       and then (for all J in Item'Range =>
                   To_C'Result (size_t (J - Item'First)) = To_C (Item (J)))
       and then (if Append_Nul then To_C'Result (To_C'Result'Last) = wide_nul);

   function To_Ada
     (Item     : wchar_array;
      Trim_Nul : Boolean := True) return Wide_String
   with
     Pre  => (if Trim_Nul then
                Is_Nul_Terminated (Item)
                  and then C_Length_Ghost (Item) <= size_t (Natural'Last)
              else
                Item'Last - Item'First < size_t (Natural'Last)),
     Post => To_Ada'Result'First = 1
       and then To_Ada'Result'Length =
         (if Trim_Nul then C_Length_Ghost (Item) else Item'Length)
       and then (for all J in To_Ada'Result'Range =>
                   To_Ada'Result (J) =
                     To_Ada (Item (size_t (J) - 1 + Item'First)));

   procedure To_C
     (Item       : Wide_String;
      Target     : out wchar_array;
      Count      : out size_t;
      Append_Nul : Boolean := True)
   with
     Relaxed_Initialization => Target,
     Pre  => Target'Length >=
       (if Append_Nul then Item'Length + 1 else Item'Length),
     Post => Count = (if Append_Nul then Item'Length + 1 else Item'Length)
       and then
         (if Count /= 0 then
           Target (Target'First .. Target'First + (Count - 1))'Initialized)
       and then
         (for all J in Item'Range =>
           Target (Target'First + size_t (J - Item'First)) = To_C (Item (J)))
       and then
         (if Append_Nul then Target (Target'First + (Count - 1)) = wide_nul);

   procedure To_Ada
     (Item     : wchar_array;
      Target   : out Wide_String;
      Count    : out Natural;
      Trim_Nul : Boolean := True)
   with
     Relaxed_Initialization => Target,
     Pre  => (if Trim_Nul then
                Is_Nul_Terminated (Item)
                  and then C_Length_Ghost (Item) <= size_t (Target'Length)
              else
                Item'Last - Item'First < size_t (Target'Length)),
     Post => Count =
         (if Trim_Nul then Natural (C_Length_Ghost (Item)) else Item'Length)
       and then
         (if Count /= 0 then
           Target (Target'First .. Target'First + (Count - 1))'Initialized)
       and then
         (for all J in Target'First .. Target'First + (Count - 1) =>
           Target (J) =
             To_Ada (Item (size_t (J - Target'First) + Item'First)));

   Terminator_Error : exception;

   --  The remaining declarations are for Ada 2005 (AI-285)

   --  ISO/IEC 10646:2003 compatible types defined by SC22/WG14 document N1010

   type char16_t is new Wide_Character;
   pragma Ada_05 (char16_t);

   char16_nul : constant char16_t := char16_t'Val (0);
   pragma Ada_05 (char16_nul);

   --  To_C and To_Ada provide mappings between the Ada and C 16-bit character
   --  types.

   function To_C (Item : Wide_Character) return char16_t
   with
     Post => To_C'Result = char16_t (Item);
   pragma Ada_05 (To_C);

   function To_Ada (Item : char16_t) return Wide_Character
   with
     Post => To_Ada'Result = Wide_Character (Item);
   pragma Ada_05 (To_Ada);

   type char16_array is array (size_t range <>) of aliased char16_t;
   pragma Ada_05 (char16_array);

   function Is_Nul_Terminated (Item : char16_array) return Boolean
   with
     Post => Is_Nul_Terminated'Result = (for some C of Item => C = char16_nul);
   pragma Ada_05 (Is_Nul_Terminated);
   --  The result of Is_Nul_Terminated is True if Item contains char16_nul, and
   --  is False otherwise.

   --  The To_C and To_Ada subprograms that convert between Wide_String and
   --  char16_array have analogous effects to the To_C and To_Ada subprograms
   --  that convert between String and char_array, except that char16_nul is
   --  used instead of nul.

   function C_Length_Ghost (Item : char16_array) return size_t
   with
     Ghost,
     Pre  => Is_Nul_Terminated (Item),
     Post => C_Length_Ghost'Result <= Item'Last - Item'First
       and then Item (Item'First + C_Length_Ghost'Result) = char16_nul
       and then (for all J in Item'First .. Item'First + C_Length_Ghost'Result
                   when J /= Item'First + C_Length_Ghost'Result =>
                     Item (J) /= char16_nul);
   --  Ghost function to compute the length of a char16_array up to the first
   --  char16_nul character.

   function To_C
     (Item       : Wide_String;
      Append_Nul : Boolean := True) return char16_array
   with
     Pre  => not (Append_Nul = False and then Item'Length = 0),
     Post => To_C'Result'First = 0
       and then To_C'Result'Length =
         (if Append_Nul then Item'Length + 1 else Item'Length)
       and then (for all J in Item'Range =>
                   To_C'Result (size_t (J - Item'First)) = To_C (Item (J)))
       and then
         (if Append_Nul then To_C'Result (To_C'Result'Last) = char16_nul);
   pragma Ada_05 (To_C);

   function To_Ada
     (Item     : char16_array;
      Trim_Nul : Boolean := True) return Wide_String
   with
     Pre  => (if Trim_Nul then
                Is_Nul_Terminated (Item)
                  and then C_Length_Ghost (Item) <= size_t (Natural'Last)
              else
                Item'Last - Item'First < size_t (Natural'Last)),
     Post => To_Ada'Result'First = 1
       and then To_Ada'Result'Length =
         (if Trim_Nul then C_Length_Ghost (Item) else Item'Length)
       and then (for all J in To_Ada'Result'Range =>
                   To_Ada'Result (J) =
                     To_Ada (Item (size_t (J) - 1 + Item'First)));
   pragma Ada_05 (To_Ada);

   procedure To_C
     (Item       : Wide_String;
      Target     : out char16_array;
      Count      : out size_t;
      Append_Nul : Boolean := True)
   with
     Relaxed_Initialization => Target,
     Pre  => Target'Length >=
       (if Append_Nul then Item'Length + 1 else Item'Length),
     Post => Count = (if Append_Nul then Item'Length + 1 else Item'Length)
       and then
         (if Count /= 0 then
           Target (Target'First .. Target'First + (Count - 1))'Initialized)
       and then
         (for all J in Item'Range =>
           Target (Target'First + size_t (J - Item'First)) = To_C (Item (J)))
       and then
         (if Append_Nul then Target (Target'First + (Count - 1)) = char16_nul);
   pragma Ada_05 (To_C);

   procedure To_Ada
     (Item     : char16_array;
      Target   : out Wide_String;
      Count    : out Natural;
      Trim_Nul : Boolean := True)
   with
     Relaxed_Initialization => Target,
     Pre  => (if Trim_Nul then
                Is_Nul_Terminated (Item)
                  and then C_Length_Ghost (Item) <= size_t (Target'Length)
              else
                Item'Last - Item'First < size_t (Target'Length)),
     Post => Count =
         (if Trim_Nul then Natural (C_Length_Ghost (Item)) else Item'Length)
       and then
         (if Count /= 0 then
           Target (Target'First .. Target'First + (Count - 1))'Initialized)
       and then
         (for all J in Target'First .. Target'First + (Count - 1) =>
           Target (J) =
             To_Ada (Item (size_t (J - Target'First) + Item'First)));
   pragma Ada_05 (To_Ada);

   type char32_t is new Wide_Wide_Character;
   pragma Ada_05 (char32_t);

   char32_nul : constant char32_t := char32_t'Val (0);
   pragma Ada_05 (char32_nul);

   --  To_C and To_Ada provide mappings between the Ada and C 32-bit character
   --  types.

   function To_C (Item : Wide_Wide_Character) return char32_t
   with
     Post => To_C'Result = char32_t (Item);
   pragma Ada_05 (To_C);

   function To_Ada (Item : char32_t) return Wide_Wide_Character
   with
     Post => To_Ada'Result = Wide_Wide_Character (Item);
   pragma Ada_05 (To_Ada);

   type char32_array is array (size_t range <>) of aliased char32_t;
   pragma Ada_05 (char32_array);

   function Is_Nul_Terminated (Item : char32_array) return Boolean
   with
     Post => Is_Nul_Terminated'Result = (for some C of Item => C = char32_nul);
   pragma Ada_05 (Is_Nul_Terminated);
   --  The result of Is_Nul_Terminated is True if Item contains char32_nul, and
   --  is False otherwise.

   function C_Length_Ghost (Item : char32_array) return size_t
   with
     Ghost,
     Pre  => Is_Nul_Terminated (Item),
     Post => C_Length_Ghost'Result <= Item'Last - Item'First
       and then Item (Item'First + C_Length_Ghost'Result) = char32_nul
       and then (for all J in Item'First .. Item'First + C_Length_Ghost'Result
                   when J /= Item'First + C_Length_Ghost'Result =>
                     Item (J) /= char32_nul);
   --  Ghost function to compute the length of a char32_array up to the first
   --  char32_nul character.

   --  The To_C and To_Ada subprograms that convert between Wide_Wide_String
   --  and char32_array have analogous effects to the To_C and To_Ada
   --  subprograms that convert between String and char_array, except
   --  that char32_nul is used instead of nul.

   function To_C
     (Item       : Wide_Wide_String;
      Append_Nul : Boolean := True) return char32_array
   with
     Pre  => not (Append_Nul = False and then Item'Length = 0),
     Post => To_C'Result'First = 0
       and then To_C'Result'Length =
         (if Append_Nul then Item'Length + 1 else Item'Length)
       and then (for all J in Item'Range =>
                   To_C'Result (size_t (J - Item'First)) = To_C (Item (J)))
       and then
         (if Append_Nul then To_C'Result (To_C'Result'Last) = char32_nul);
   pragma Ada_05 (To_C);

   function To_Ada
     (Item     : char32_array;
      Trim_Nul : Boolean := True) return Wide_Wide_String
   with
     Pre  => (if Trim_Nul then
                Is_Nul_Terminated (Item)
                  and then C_Length_Ghost (Item) <= size_t (Natural'Last)
              else
                Item'Last - Item'First < size_t (Natural'Last)),
     Post => To_Ada'Result'First = 1
       and then To_Ada'Result'Length =
         (if Trim_Nul then C_Length_Ghost (Item) else Item'Length)
       and then (for all J in To_Ada'Result'Range =>
                   To_Ada'Result (J) =
                     To_Ada (Item (size_t (J) - 1 + Item'First)));
   pragma Ada_05 (To_Ada);

   procedure To_C
     (Item       : Wide_Wide_String;
      Target     : out char32_array;
      Count      : out size_t;
      Append_Nul : Boolean := True)
   with
     Relaxed_Initialization => Target,
     Pre  => Target'Length >=
       (if Append_Nul then Item'Length + 1 else Item'Length),
     Post => Count = (if Append_Nul then Item'Length + 1 else Item'Length)
       and then
         (if Count /= 0 then
           Target (Target'First .. Target'First + (Count - 1))'Initialized)
       and then
         (for all J in Item'Range =>
           Target (Target'First + size_t (J - Item'First)) = To_C (Item (J)))
       and then
         (if Append_Nul then Target (Target'First + (Count - 1)) = char32_nul);
   pragma Ada_05 (To_C);

   procedure To_Ada
     (Item     : char32_array;
      Target   : out Wide_Wide_String;
      Count    : out Natural;
      Trim_Nul : Boolean := True)
   with
     Relaxed_Initialization => Target,
     Pre  => (if Trim_Nul then
                Is_Nul_Terminated (Item)
                  and then C_Length_Ghost (Item) <= size_t (Target'Length)
              else
                Item'Last - Item'First < size_t (Target'Length)),
     Post => Count =
         (if Trim_Nul then Natural (C_Length_Ghost (Item)) else Item'Length)
       and then
         (if Count /= 0 then
           Target (Target'First .. Target'First + (Count - 1))'Initialized)
       and then
         (for all J in Target'First .. Target'First + (Count - 1) =>
           Target (J) =
             To_Ada (Item (size_t (J - Target'First) + Item'First)));
   pragma Ada_05 (To_Ada);

end Interfaces.C;
