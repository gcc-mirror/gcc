/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */

int p[4096], q[4096];

void foo (int n)
{
  int i;
  for (i = 0; i < n; ++i)
    {
      p[i*4+0] = q[i*8+0] + q[i*8+4];
      p[i*4+1] = q[i*8+1] + q[i*8+5];
      p[i*4+2] = q[i*8+2] + q[i*8+6];
      p[i*4+3] = q[i*8+3] + q[i*8+7];
    }
}

/* { dg-final { scan-tree-dump "vectorizing stmts using SLP" "vect" { xfail { vect_variable_length && { ! vect_strided8 } } } } } */
/* { dg-final { scan-tree-dump "vectorized 1 loops" "vect" } } */
