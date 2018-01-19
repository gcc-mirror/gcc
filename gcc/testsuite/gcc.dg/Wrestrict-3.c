/* Test to verify that the call below with the out-of-bounds offset
   doesn't trigger an internal assertion and is diagnosed.
   { dg-do compile }
   { dg-options "-O2 -Wrestrict" } */

#define DIFF_MAX   __PTRDIFF_MAX__

void test_no_ice (int *d, __PTRDIFF_TYPE__ i, __SIZE_TYPE__ n)
{
  if (i < DIFF_MAX / sizeof *d - 1 || DIFF_MAX / sizeof *d + 2 < i)
    i = DIFF_MAX / sizeof *d - 1;

  if (n < DIFF_MAX)
    n = DIFF_MAX / sizeof *d;

  __builtin_strncpy ((char*)(d + i), (char*)d, n);   /* { dg-warning "\\\[-Wrestrict]" } */
}
