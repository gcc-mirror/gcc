/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-optimized" } */
/* { dg-require-effective-target int128 } */

/* We can not get rid of comparison in tests below because of
   potential overflow exception.

   TODO: enable when -fno-trapping-math.  */

int foo(__int128_t x) {
  /* { dg-final { scan-tree-dump "\\(float\\)" "optimized" } } */
  return (float) x != 0;
}
