/* { dg-do compile } */

void
volarr_cpy(char *d, volatile char *s)
{
  int i;
  
  for (i = 0; i < 16; i++)
    d[i] = s[i];
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { xfail *-*-* } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */

