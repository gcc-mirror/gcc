/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */

int a[1024];

void foo (void)
{
  for (int i = 0; i < 1020; i += 4)
    {
      int suma = a[i];
      int sumb = a[i+1];
      int sumc = a[i+2];
      int sumd = a[i+3];
      for (unsigned j = 0; j < 77; ++j)
        {
          suma = (suma ^ i) + 1;
          sumb = (sumb ^ i) + 2;
          sumc = (sumc ^ i) + 3;
          sumd = (sumd ^ i) + 4;
        }
      a[i] = suma;
      a[i+1] = sumb;
      a[i+2] = sumc;
      a[i+3] = sumd;
    }
}

/* We should vectorize this outer loop with SLP.  */
/* { dg-final { scan-tree-dump "OUTER LOOP VECTORIZED" "vect" } } */
/* We don't yet support SLP inductions for variable length vectors.  */
/* { dg-final { scan-tree-dump "vectorizing stmts using SLP" "vect" { xfail vect_variable_length } } } */
/* { dg-final { scan-tree-dump-not "VEC_PERM_EXPR" "vect" } } */
