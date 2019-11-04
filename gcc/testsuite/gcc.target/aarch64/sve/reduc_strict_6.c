/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize" } */

double mat[100][4];
double mat2[100][8];
double mat3[100][12];
double mat4[100][3];

void
slp_non_chained_reduc (int n, double * restrict out)
{
  for (int i = 0; i < 3; i++)
    out[i] = 0;

  for (int i = 0; i < n; i++)
    {
      out[0] = out[0] + mat4[i][0];
      out[1] = out[1] + mat4[i][1];
      out[2] = out[2] + mat4[i][2];
    }
}

/* { dg-final { scan-assembler-times {\tld3d\t} 1 } } */
/* { dg-final { scan-assembler-times {\tfadda\td[0-9]+, p[0-7], d[0-9]+, z[0-9]+\.d} 3 } } */
