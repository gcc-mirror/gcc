/* Test initialization by UTF-8 string literal in C23.  */
/* { dg-do compile } */
/* { dg-require-effective-target wchar } */
/* { dg-options "-std=c23" } */

typedef __CHAR8_TYPE__  char8_t;
typedef __CHAR16_TYPE__ char16_t;
typedef __CHAR32_TYPE__ char32_t;
typedef __WCHAR_TYPE__  wchar_t;

/* Test that char, signed char, unsigned char, and char8_t arrays can be
   initialized by a UTF-8 string literal.  */
const char cbuf1[] = u8"text";
const char cbuf2[] = { u8"text" };
const signed char scbuf1[] = u8"text";
const signed char scbuf2[] = { u8"text" };
const unsigned char ucbuf1[] = u8"text";
const unsigned char ucbuf2[] = { u8"text" };
const char8_t c8buf1[] = u8"text";
const char8_t c8buf2[] = { u8"text" };

/* Test that a diagnostic is issued for attempted initialization of
   other character types by a UTF-8 string literal.  */
const char16_t c16buf1[] = u8"text";		/* { dg-error "from a string literal with type array of .unsigned char." } */
const char16_t c16buf2[] = { u8"text" };	/* { dg-error "from a string literal with type array of .unsigned char." } */
const char32_t c32buf1[] = u8"text";		/* { dg-error "from a string literal with type array of .unsigned char." } */
const char32_t c32buf2[] = { u8"text" };	/* { dg-error "from a string literal with type array of .unsigned char." } */
const wchar_t wbuf1[] = u8"text";		/* { dg-error "from a string literal with type array of .unsigned char." } */
const wchar_t wbuf2[] = { u8"text" };		/* { dg-error "from a string literal with type array of .unsigned char." } */

/* Test that char8_t arrays can be initialized by an ordinary string
   literal.  */
const char8_t c8buf3[] = "text";
const char8_t c8buf4[] = { "text" };
