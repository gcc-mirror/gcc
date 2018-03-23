/* PR tree-optimization/83508 - c-c++-common/Wrestrict.c fails since r255836
   Test to verify that only one of -Wrestrict and -Wstringop-overflow is
   issued for a problem where either would be appropriate.
   { dg-do compile }
   { dg-options "-O2 -Wrestrict -Wstringop-overflow" } */

#define DIFF_MAX __PTRDIFF_MAX__

typedef __PTRDIFF_TYPE__ ptrdiff_t;
typedef __SIZE_TYPE__ size_t;

void sink (void*);

void f (ptrdiff_t i, size_t n)
{
  if (i < DIFF_MAX - 2 || DIFF_MAX - 1 > i)
    i = DIFF_MAX - 2;

  if (n < 4 || 5 < n)
    n = 4;

  char a[8] = "012";

  /* The following could very well be diagnosed by -Wstringop-overflow
     instead but there's no way to verify that only one of the two
     warnings is issued and the choice of -Wrestrict simply reflects
     the fact that -Wrestrict runs before -Wstringop-overflow.  */
  __builtin_strncpy (a + i, a, n);   /* { dg-warning "\\\[-Wrestrict]" } */
  sink (a);
}
