/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */

unsigned
frd (unsigned *p, unsigned *lastone)
{
  unsigned sum = 0;
  for (; p <= lastone; p += 16)
    sum += p[0] + p[1] + p[2] + p[3] + p[4] + p[5] + p[6] + p[7]
           + p[8] + p[9] + p[10] + p[11] + p[12] + p[13] + p[14] + p[15];
  return sum;
}

/* { dg-final { scan-tree-dump "Starting SLP discovery of reduction chain" "vect" } } */
/* { dg-final { scan-tree-dump-not "SLP discovery of reduction chain failed" "vect" } } */
/* { dg-final { scan-tree-dump "optimized: loop vectorized" "vect" } } */
