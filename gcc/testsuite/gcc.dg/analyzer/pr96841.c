/* { dg-additional-options "-Wno-analyzer-too-complex -O1 -Wno-builtin-declaration-mismatch" } */

int
l8 (void);

__SIZE_TYPE__
malloc (__SIZE_TYPE__);

void
th (int *);

void
bv (__SIZE_TYPE__ ny, int ***mf)
{
  while (l8 ())
    {
      *mf = 0;
      (*mf)[ny] = (int *) malloc (sizeof (int));
      th ((*mf)[ny]);
    }
}
