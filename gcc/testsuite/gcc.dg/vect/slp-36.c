/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target vect_shift } */

#define N 32

/* All the loops are vectorizable on platforms with vector shift argument.  */

void
test_1 (void)
{
  static unsigned int bm[N];
  static unsigned int cm[N];
  int j;

  /* Vectorizable on platforms with scalar shift argument.  */
  for (j = 0; j < N/2; j++)
   {
    bm[2*j] <<= 8;
    bm[2*j+1] <<= 8;
   }

  /* Not vectorizable on platforms with scalar shift argument.  */
  for (j = 0; j < N/2; j++)
   {
    cm[2*j] <<= 8;
    cm[2*j+1] <<= 7;
   }
}

void
test_2 (int a, int b)
{
  static unsigned int bm[N];
  static unsigned int cm[N];
  int j;

  /* Vectorizable on platforms with scalar shift argument.  */
  for (j = 0; j < N/2; j++)
   {
    bm[2*j] <<= a;
    bm[2*j+1] <<= a;
   }

  /* Not vectorizable on platforms with scalar shift argument.  */
  for (j = 0; j < N/2; j++)
   {
    cm[2*j] <<= a;
    cm[2*j+1] <<= b;
   }
}

void
test_3 (void)
{
 static unsigned int bm[N];
 int am[N];
 int j;

 /* Not vectorizable on platforms with scalar shift argument.  */
 for (j = 0; j < N/2; j++)
   {
    bm[2*j] <<= am[j];
    bm[2*j+1] <<= am[j];
   }

 /* Not vectorizable on platforms with scalar shift argument.  */
 for (j = 0; j < N/2; j++)
   {
    bm[2*j] <<= am[2*j];
    bm[2*j+1] <<= am[2*j+1];
   }

}

/* { dg-final { cleanup-tree-dump "vect" } } */
