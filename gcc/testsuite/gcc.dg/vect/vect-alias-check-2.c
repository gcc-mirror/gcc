/* { dg-do compile } */
/* { dg-require-effective-target vect_int_mult } */

int a [128];
int b[128] = {0};

int foo (void)
{
  int k;

  for(k=0; k<64; k++)
  {
    b[k] = 10 - b[127-k];
    a[k] = b[k] * 3;
    a[127-k] = b[127-k] * 2;
  }
}

/* { dg-final { scan-tree-dump-not "versioning for alias checks." "vect" } } */
