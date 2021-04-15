/* PR middle-end/86058 - TARGET_MEM_REF causing incorrect message for
   -Wmaybe-uninitialized warning
   The test fails on a number of non-x86 targets due to pr100073.
   { dg-do compile { target i?86-*-* x86_64-*-* } }
   { dg-options "-O2 -Wuninitialized -Wmaybe-uninitialized" } */

extern void foo (int *);

void zip (int *out, int indx)
{
  int arr[10];

  for (int i = 0; i < indx; ++i)
    out[i] = arr[i] + 1;  // { dg-warning "'arr\\\[i]' may be used uninitialized" "pr99944" { xfail *-*-* } }
                          // { dg-warning "'arr' may be used uninitialized" "actual" { target *-*-* } .-1 }

  foo (arr);
  foo (out);
}
