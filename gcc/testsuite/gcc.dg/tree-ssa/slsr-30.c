/* Verify straight-line strength reduction fails for simple integer addition
   with casts thrown in when -fwrapv is used.  */

/* { dg-do compile { target { long_neq_int } } } */
/* { dg-options "-O3 -fdump-tree-dom2 -fwrapv" } */

long
f (int s, long c)
{
  int a1, a2, a3;
  long x1, x2, x3, x;

  a1 = 2 * s;
  x1 = c + a1;
  a2 = 4 * s;
  x2 = c + a2;
  a3 = 6 * s;
  x3 = c + a3;
  x = x1 + x2 + x3;
  return x;
}

/* { dg-final { scan-tree-dump-times " \\* " 3 "dom2" } } */
/* { dg-final { cleanup-tree-dump "dom2" } } */
