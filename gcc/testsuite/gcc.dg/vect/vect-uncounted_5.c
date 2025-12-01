/* Loop with undetermined increment at compile-time is treated as uncounted.  */
/* Adapted from pr102572.cc  */
/* { dg-add-options vect_early_break } */
/* { dg-do compile } */
/* { dg-require-effective-target vect_early_break } */
/* { dg-require-effective-target vect_int } */

int a, b, c, f;
void g(bool h, int d[][5])
{
  int k = 0;
  for (short i = f; i; i += 1)
    {
      a = h && d[0][i];
      for (int j = 0; j < 4; j += c)
	b++;
    }
}
/* { dg-final { scan-tree-dump "Loop being analyzed as uncounted." "vect" } } */
/* { dg-final { scan-tree-dump "vectorized 1 loops in function" "vect" } } */
