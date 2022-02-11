/* { dg-do compile } */

int r;

void
foo (short int *s, short int *d1, short int *d2, int z)
{
  int *a;

  while (z < 1)
    {
      int i;

      i = *s++ - (*d1++ + *d2++);
      r += a[i];
      i = *s++ - (*d1++ + *d2++);
      r += a[i];
      ++z;
    }
}
