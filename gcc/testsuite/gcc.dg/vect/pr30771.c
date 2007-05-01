/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */

int a[128];

int
main()
{
  short i;

  for (i=0; i<64; i++){
    a[i] = (int)i;
  }
  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target vect_unpack } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
