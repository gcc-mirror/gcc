/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */

int
foo (int * restrict a, int n)
{
  int i, result = 0;

  a = __builtin_assume_aligned (a, __BIGGEST_ALIGNMENT__);
  for (i = 0; i < n * 16; i++)
    result += a[i];
  return result;
}

/* { dg-final { scan-tree-dump-not "epilog loop required" "vect" } } */
