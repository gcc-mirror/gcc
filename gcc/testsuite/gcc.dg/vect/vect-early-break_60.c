/* { dg-add-options vect_early_break } */
/* { dg-do compile } */
/* { dg-require-effective-target vect_early_break } */
/* { dg-require-effective-target vect_float } */

extern void abort();
float a[1024], b[1024], c[1024], d[1024];
_Bool k[1024];

int main ()
{
  char i;
  for (i = 0; i < 1024; i++)
    if (k[i] != (i == 0))
      abort ();
}

/* { dg-final { scan-tree-dump "vectorized 1 loops in function" "vect" { xfail *-*-* } } } */
