/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */

int b(int n, unsigned char *a)
{
  int d = 0;
  a = __builtin_assume_aligned (a, __BIGGEST_ALIGNMENT__);
  for (int c = 0; c < n; ++c)
    d |= a[c];
  return d;
}

/* { dg-final { scan-tree-dump "vectorized 1 loops" "vect" { target { vect_unpack && { ! vect_no_bitwise } } } } } */
