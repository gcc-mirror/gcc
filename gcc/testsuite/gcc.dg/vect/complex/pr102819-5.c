/* { dg-do compile } */
/* { dg-require-effective-target vect_float } */
/* { dg-add-options arm_v8_3a_complex_neon } */

float f[12][100];

void good2()
{
  for (int r = 0; r < 100; r += 2)
    {
      int i = r + 1;
      f[0][r] = f[1][r] * (f[2][r] + 1) - f[1][i] * (f[2][i] + 1);
      f[0][i] = f[1][r] * (f[2][i] + 1) + f[1][i] * (f[2][r] + 1);
    }
}

/* { dg-final { scan-tree-dump "Found COMPLEX_MUL" "vect" { target { vect_float } } } } */

