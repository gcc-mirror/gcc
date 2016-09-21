/* { dg-do compile } */
/* { dg-options "-std=c99 -Wformat -Wformat-length=2 -ftrack-macro-expansion=0" } */

/* When debugging, define LINE to the line number of the test case to exercise
   and avoid exercising any of the others.  The buffer and objsize macros
   below make use of LINE to avoid warnings for other lines.  */
#ifndef LINE
# define LINE 0
#endif

char buffer [256];
extern char *ptr;

#define buffer(size)							\
  (!LINE || __LINE__ == LINE ? buffer + sizeof buffer - size : ptr)

#define objsize(size)  (!LINE || __LINE__ == LINE ? size : __SIZE_MAX__ / 2)

typedef __SIZE_TYPE__ size_t;

#if !__cplusplus
typedef __WCHAR_TYPE__ wchar_t;
#endif

typedef unsigned char UChar;

#define T(size, fmt, ...)				\
  __builtin_sprintf (buffer (size), fmt, __VA_ARGS__)

__builtin_va_list va;

/* Exercise buffer overflow detection with const string arguments.  */

void test_s_const (void)
{
    /* Wide string literals are handled slightly differently than
       at level 1.  At level 1, each wide character is assumed to
       convert into a single byte.  At level 2, they are assumed
       to convert into at least one byte.  */
  T (0, "%ls",      L"");       /* { dg-warning "nul past the end" } */
  T (1, "%ls",      L"");
  T (1, "%ls",      L"\0");
  T (1, "%1ls",     L"");       /* { dg-warning "nul past the end" } */

  T (0, "%*ls",  0, L"");       /* { dg-warning "nul past the end" } */
  T (1, "%*ls",  0, L"");
  T (1, "%*ls",  0, L"\0");
  T (1, "%*ls",  1, L"");       /* { dg-warning "nul past the end" } */

  T (1, "%ls",      L"1");      /* { dg-warning "nul past the end" } */
  T (1, "%.0ls",    L"1");
  T (2, "%.0ls",    L"1");
  T (2, "%.1ls",    L"1");

  /* The "%.2ls" directive below will write at a minimum 1 byte (because
     L"1" is known and can be assumed to convert to at least one multibyte
     character), and at most 2 bytes because of the precision.  Since its
     output is explicitly bounded it is diagnosed.  */
  T (2, "%.2ls",    L"1");      /* { dg-warning "nul past the end" } */
  T (2, "%.*ls", 2, L"1");      /* { dg-warning "nul past the end" } */

  /* The following three are constrained by the precision to at most
     that many bytes of the converted wide string plus a terminating NUL.  */
  T (2, "%.0ls",    L"1");
  T (2, "%.1ls",    L"1");
  T (3, "%.2ls",    L"1");
}


struct Arrays {
  char a1 [1];
  char a2 [2];
  char a3 [3];
  char a4 [4];
  char a0 [0];
  char ax [];
};

/* Exercise buffer overflow detection with non-const string arguments.  */

void test_s_nonconst (const char *s, const wchar_t *ws, struct Arrays *a)
{
  T (0, "%s",   s);             /* { dg-warning "into a region" "sprintf transformed into strcpy" { xfail *-*-*-* } } */
  T (1, "%s",   s);             /* { dg-warning "nul past the end" "sprintf transformed into strcpy" { xfail *-*-*-* } } */
  T (1, "%1s",  s);             /* { dg-warning "nul past the end" } */
  T (1, "%.0s", s);
  T (1, "%.1s", s);             /* { dg-warning "writing a terminating nul" } */

  T (1, "%ls",  ws);            /* { dg-warning "writing a terminating nul" } */

  /* Verify that the size of the array is used in lieu of its length.
     The minus sign disables GCC's sprintf to strcpy transformation.  */
  T (1, "%-s", a->a1);          /* { dg-warning "nul past the end" } */

  /* In the following test, since the length of the strings isn't known,
     their type (the array) is used to bound the maximum length to 1,
     which means the "%-s" directive would not overflow the buffer,
     but it would leave no room for the terminating nul.  */
  T (1, "%-s", a->a2);          /* { dg-warning "writing a terminating nul" } */

  /* Unlike in the test above, since the length of the string is bounded
     by the array type to at most 2, the "^-s" directive is diagnosed firts,
     preventing the diagnostic about the terminatinb nul.  */
  T (1, "%-s", a->a3);          /* { dg-warning "directive writing between 1 and 2 bytes" } */

  /* The length of a zero length array and flexible array member is
     unknown and at leve 2 assumed to be at least 1.  */
  T (1, "%-s", a->a0);          /* { dg-warning "nul past the end" } */
  T (1, "%-s", a->ax);          /* { dg-warning "nul past the end" } */

  T (2, "%-s", a->a0);
  T (2, "%-s", a->ax);
}

  /* Exercise buffer overflow detection with non-const integer arguments.  */

void test_hh_nonconst (int x)
{
  T (1, "%hhi",         x);     /* { dg-warning "into a region" } */
  T (2, "%hhi",         x);     /* { dg-warning "into a region" } */
  T (3, "%hhi",         x);     /* { dg-warning "into a region" } */
  T (4, "%hhi",         x);     /* { dg-warning "may write a terminating nul past the end of the destination" } */
}

void test_h_nonconst (int x)
{
  extern UChar uc;

  T (1, "%hi",         uc);     /* { dg-warning "into a region" } */
  T (2, "%hi",         uc);     /* { dg-warning "into a region" } */
  /* Formatting an 8-bit unsigned char as a signed short (or any other
     type with greater precision) can write at most 3 characters.  */
  T (3, "%hi",         uc);     /* { dg-warning "terminating nul past" } */
  T (4, "%hi",         uc);

  /* Verify that the same thing works when the int argument is cast
     to unsigned char.  */
  T (1, "%hi",   (UChar)x);     /* { dg-warning "into a region" } */
  T (2, "%hi",   (UChar)x);     /* { dg-warning "into a region" } */
  T (3, "%hi",   (UChar)x);     /* { dg-warning "may write a terminating nul past the end of the destination" } */
  T (4, "%hi",   (UChar)x);
}

void test_i_nonconst (int x)
{
  extern UChar uc;

  T (1, "%i",          uc);     /* { dg-warning "into a region" } */
  T (2, "%i",          uc);     /* { dg-warning "into a region" } */
  T (3, "%i",          uc);     /* { dg-warning "terminating nul past" } */
  T (4, "%i",          uc);

  T (1, "%i",    (UChar)x);     /* { dg-warning "into a region" } */
  T (2, "%i",    (UChar)x);     /* { dg-warning "into a region" } */
  T (3, "%i",    (UChar)x);     /* { dg-warning "terminating nul past" } */
  T (4, "%i",    (UChar)x);

  /* Verify the same thing using a bit-field.  */
  extern struct {
    unsigned int  b1: 1;
    unsigned int  b2: 2;
    unsigned int  b3: 3;
    unsigned int  b4: 4;
	     int sb4: 4;
    unsigned int  b5: 5;
    unsigned int  b6: 6;
    unsigned int  b7: 7;
    unsigned int  b8: 8;
  } bf, abf[], *pbf;

  T (1, "%i",       bf.b1);     /* { dg-warning "nul past the end" } */
  T (1, "%i",  abf [x].b1);     /* { dg-warning "nul past the end" } */
  T (1, "%i",     pbf->b1);     /* { dg-warning "nul past the end" } */
  /* A one bit bit-field can only be formatted as '0' or '1'.  Similarly,
     two- and three-bit bit-fields can only be formatted as a single
     decimal digit.  */
  T (2, "%i",       bf.b1);
  T (2, "%i",  abf [x].b1);
  T (2, "%i",     pbf->b1);
  T (2, "%i",       bf.b2);
  T (2, "%i",  abf [x].b2);
  T (2, "%i",     pbf->b2);
  T (2, "%i",       bf.b3);
  T (2, "%i",  abf [x].b3);
  T (2, "%i",     pbf->b3);
  /* A four-bit bit-field can be formatted as either one or two digits.  */
  T (2, "%i",       bf.b4);     /* { dg-warning "nul past the end" } */
  T (2, "%i",  abf [x].b4);     /* { dg-warning "nul past the end" } */
  T (2, "%i",     pbf->b4);     /* { dg-warning "nul past the end" } */

  T (3, "%i",       bf.b4);
  T (3, "%i",     pbf->b4);
  T (3, "%i",       bf.b5);
  T (3, "%i",     pbf->b5);
  T (3, "%i",       bf.b6);
  T (3, "%i",     pbf->b6);
  T (3, "%i",       bf.b7);     /* { dg-warning "nul past the end" } */
  T (3, "%i",     pbf->b7);     /* { dg-warning "nul past the end" } */

  T (1, "%i",       bf.b8);     /* { dg-warning "into a region" } */
  T (2, "%i",       bf.b8);     /* { dg-warning "into a region" } */
  /* Formatting an 8-bit unsigned char as a signed short (or any other
     type with greater precision) int can write at most 3 characters.  */
  T (3, "%i",       bf.b8);     /* { dg-warning "terminating nul past" } */
  T (4, "%i",       bf.b8);

  T (1, "%i",       bf.b8);     /* { dg-warning "into a region" } */
  T (2, "%i",       bf.b8);     /* { dg-warning "into a region" } */
  T (3, "%i",       bf.b8);     /* { dg-warning "terminating nul past" } */

  T (2, "%i",      bf.sb4);     /* { dg-warning "terminating nul past" } */
  T (3, "%i",      bf.sb4);
  T (4, "%i",      bf.sb4);
}
