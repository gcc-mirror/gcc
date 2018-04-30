/* { dg-do compile { target { ! x32 } } }
   { dg-require-effective-target mempcpy }
   { dg-options "-O2 -Wrestrict -fcheck-pointer-bounds -mmpx" } */

#define USE_GNU
#include "../../gcc.dg/strlenopt.h"

/* There is no BUILT_IN_ST{P,R}NCPY_CHKP or BUILT_IN_STRNCAT_CHKP
   so the test for them below are XFAIL.  */
char *stpncpy (char *__restrict, const char *__restrict, size_t);
char *strncpy (char *__restrict, const char *__restrict, size_t);
char *strncat (char *__restrict, const char *__restrict, size_t);


char a[8];

void test_memcpy (void)
{
  memcpy (a, a + 1, 3);   /* { dg-warning ".memcpy\.chkp. accessing 3 bytes at offsets 0 and 1 overlaps 2 bytes at offset 1" } */
}

void test_memmove (void)
{
  memmove (a, a + 1, 3);
}

void* test_mempcpy (void)
{
  return mempcpy (a, a + 1, 3);   /* { dg-warning ".mempcpy\.chkp. accessing 3 bytes at offsets 0 and 1 overlaps 2 bytes at offset 1" } */
}

char* test_stpcpy (void)
{
  strcpy (a, "0123456");
  return stpcpy (a, a + 2);   /* { dg-warning ".stpcpy\.chkp. accessing 6 bytes at offsets 0 and 2 overlaps 4 bytes at offset 2" } */
}

char* test_stpncpy (void)
{
  strcpy (a, "0123456");

  /* There is no BUILT_IN_STPNCPY_CHKP so this isn't handled.  */
  return stpncpy (a, a + 2, sizeof a);   /* { dg-warning ".stpcpy\.chkp. accessing 7 bytes at offsets 0 and 2 overlaps 4 bytes at offset 2" "bug 82652" { xfail *-*-* } } */
}

void test_strcpy (void)
{
  strcpy (a, "0123456");
  strcpy (a, a + 1);   /* { dg-warning ".strcpy\.chkp. accessing 7 bytes at offsets 0 and 1 overlaps 6 bytes at offset 1" } */
}

void test_strcat (int n)
{
  strcat (a, a + 3);   /* { dg-warning ".strcat\.chkp. accessing 4 or more bytes at offsets 0 and 3 may overlap 1 byte at offset 3" } */
}

void test_strncat (int n)
{
  strncat (a, a + 3, sizeof a);   /* { dg-warning ".strncat\.chkp. accessing 0 or more bytes at offsets 0 and 3 may overlap 1 byte" "bug 82652" { xfail *-*-* } } */
}

void test_strncpy (int n)
{
  strcpy (a, "0123456");

  /* There is no BUILT_IN_STRNCPY_CHKP so this isn't handled.  */
  strncpy (a, a + 2, sizeof a);   /* { dg-warning ".strncpy\.chkp. accessing 7 bytes at offsets 0 and 2 overlaps 5 bytes at offset 2" "bug 82652" { xfail *-*-* } } */
}
