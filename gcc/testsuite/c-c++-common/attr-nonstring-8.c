/* PR middle-end/85602 - -Wsizeof-pointer-memaccess for strncat with size
   of source
   { dg-do compile }
   { dg-options "-O2 -Wno-array-bounds -Wsizeof-pointer-memaccess -Wstringop-truncation -ftrack-macro-expansion=0" } */

#include "../gcc.dg/range.h"

typedef __SIZE_TYPE__ size_t;

#if __cplusplus
extern "C" {
#endif

char* strcpy (char*, const char*);
size_t strlen (const char*);
char* strncat (char*, const char*, __SIZE_TYPE__);
char* strncpy (char*, const char*, __SIZE_TYPE__);

#if __cplusplus
}
#endif

#define NONSTR __attribute__ ((nonstring))

NONSTR char nd3[3];
NONSTR char nd4[4];
NONSTR char nd5[5];

NONSTR char ns3[3];
NONSTR char ns4[4];
NONSTR char ns5[5];

NONSTR char* pns;

void sink (void*, ...);

#define T(call) sink (call)

/* Verify that for a nonstring source array of an unknown length
   a warning is issued only when the bound exceeds the array size.  */

void test_strncat_nonstring_cst (char *d)
{
  T (strncat (d, ns3, 1));
  T (strncat (d, ns3, 2));
  T (strncat (d, ns3, 3));
  T (strncat (d, ns3, sizeof ns3));
  T (strncat (d, ns3, 4));     /* { dg-warning "argument 2 declared attribute .nonstring. is smaller than the specified bound 4" } */

  T (strncat (d, ns4, 1));
  T (strncat (d, ns4, 2));
  T (strncat (d, ns4, 3));
  T (strncat (d, ns4, 4));
  T (strncat (d, ns4, sizeof ns4));
  T (strncat (d, ns4, 5));     /* { dg-warning "argument 2 declared attribute .nonstring. is smaller than the specified bound 5" } */

  T (strncat (nd3, ns3, 1));
  T (strncat (nd3, ns3, 2));
  T (strncat (nd3, ns3, 3));     /* { dg-warning "specified bound 3 equals destination size" } */
  T (strncat (nd3, ns3, 4));     /* { dg-warning "argument 2 declared attribute .nonstring. is smaller than the specified bound 4" } */
  /* { dg-warning "specified bound 4 exceeds destination size 3" "" { target *-*-* } .-1 } */

  T (strncat (d, pns, sizeof pns));   /* { dg-warning "argument to .sizeof. in .\[^\n\r\]*strncat\[^\n\r\]*. call is the same expression as the source" } */
}


void test_strncat_nonstring_var (char *d, size_t n)
{
  /* In the following the bound coulld apply to either the destination
     or the source.  The expected use of strncat() is to pass it as
     the bound DSIZE - strlen(D) - 1 so the call below is diagnosed.  */
  T (strncat (d, ns3, n));            /* { dg-warning "argument 2 declared attribute .nonstring." } */

  T (strncat (d, ns3, UR (0, 1)));
  T (strncat (d, ns3, UR (1, 2)));
  T (strncat (d, ns3, UR (2, 3)));
  T (strncat (d, ns3, UR (3, 4)));    /* { dg-warning "argument 2 declared attribute 'nonstring' may be smaller than the specified bound \\\[3, 4]" } */
  T (strncat (d, ns3, UR (4, 5)));    /* { dg-warning "argument 2 declared attribute 'nonstring' is smaller than the specified bound \\\[4, 5]" } */

  /* Verify that the call below (the intended use of strncat()) is
     also diagnosed.  */
  T (strncat (d, ns3, 256 - strlen (d) - 1));   /* { dg-warning "argument 2 declared attribute .nonstring." } */

  T (strncat (nd3, ns5, UR (0, 1)));
  T (strncat (nd3, ns5, UR (1, 2)));
  T (strncat (nd3, ns5, UR (2, 3)));
  T (strncat (nd3, ns5, UR (3, 4)));
  T (strncat (nd3, ns5, UR (4, 5)));  /* { dg-warning "specified bound between 4 and 5 exceeds destination size 3" } */

  T (strncat (nd5, ns3, UR (0, 1)));
  T (strncat (nd5, ns3, UR (1, 2)));
  T (strncat (nd5, ns3, UR (2, 3)));
  T (strncat (nd5, ns3, UR (3, 4)));  /* { dg-warning "argument 2 declared attribute 'nonstring' may be smaller than the specified bound \\\[3, 4]" } */
}

/* Verify that for a nonstring source array of a known length (i.e.,
   a nonstring array containing a nul-terminated string) a warning
   is issued only for certain truncation.

   The test cases are split up to work around bug 81343 (or one like
   it).  */

void test_strncat_string_1_1 (char *d)
{
  strcpy (ns3, "1");
  T (strncat (d, ns3, 1));    /* { dg-warning "output truncated before terminating nul copying 1 byte from a string of the same length" } */
}

void test_strncat_string_1_2 (char *d)
{
  strcpy (ns3, "1");
  T (strncat (d, ns3, 2));
}

void test_strncat_string_1_3 (char *d)
{
  strcpy (ns3, "1");
  T (strncat (d, ns3, 3));
}

void test_strncat_string_2_1 (char *d)
{
  strcpy (ns3, "12");
  T (strncat (d, ns3, 1));    /* { dg-warning "output truncated copying 1 byte from a string of length 2" } */
}

void test_strncat_string_2_2 (char *d)
{
  strcpy (ns3, "12");
  T (strncat (d, ns3, 2));    /* { dg-warning "output truncated before terminating nul copying 2 bytes from a string of the same length" } */
}

void test_strncat_string_2_3 (char *d)
{
  strcpy (ns3, "12");
  T (strncat (d, ns3, 3));
}


void test_strcncpy_nonstring_cst (char *d)
{
  T (strncpy (d, ns3, 1));
  T (strncpy (d, ns3, 2));
  T (strncpy (d, ns3, 3));
  T (strncpy (d, ns3, sizeof ns3));
  T (strncpy (d, ns3, 4));      /* { dg-warning "argument 2 declared attribute .nonstring. is smaller than the specified bound 4" } */
}
