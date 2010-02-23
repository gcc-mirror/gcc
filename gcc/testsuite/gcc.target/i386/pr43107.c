/* PR target/43107 */
/* { dg-do compile } */
/* { dg-options "-O3 -mavx" } */

extern void bar (float b[4][4]);

void
foo ()
{
  float a[4][4], b[4][4];
  int i, j;
  for (i = 0; i < 4; i++)
    {
      for (j = 0; j < 4; j++)
	a[i][j] = 0;
      for (j = 0; j < 4; j++)
	b[i][j] = a[i][j];
    }
  bar (b);
}
