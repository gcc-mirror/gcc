/* PR middle-end/71654  */
/* { dg-do link }  */
/* { dg-options "-O2" }  */

unsigned char i0, i1;

void foo (void);

int
main (void)
{
  int j = i0;
  if (j < 4)
    {
      if (i0 & 4)
        foo ();
    }

  unsigned int k = i1;
  if (k < 8)
    {
      if (i1 & 8)
        foo ();
    }

  return 0;
}

