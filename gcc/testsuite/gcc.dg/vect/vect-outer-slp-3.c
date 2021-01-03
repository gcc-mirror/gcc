/* { dg-require-effective-target vect_double } */
/* { dg-require-effective-target vect_intdouble_cvt } */

#include "tree-vect.h"

double image[40];

void __attribute__((noipa))
foo (void)
{
  for (int i = 0; i < 20; i++)
    {
      double suma = 0;
      double sumb = 0;
      int k = image[2*i];
      int l = image[2*i+1];
      for (int j = 0; j < 40; j++)
        {
          suma += k+i;
          sumb += l+i;
          k++;
          l++;
        }
      image[2*i] = suma;
      image[2*i+1] = sumb;
    }
}

int main ()
{
  check_vect ();

  for (int i = 0; i < 40; ++i)
    image[i] = 1.;

  foo ();

  for (int i = 0; i < 20; i++)
    {
      double suma = 0;
      double sumb = 0;
      int k = 1;
      int l = 1;
      for (int j = 0; j < 40; j++)
	{
          suma += k+i;
          sumb += l+i;
	  asm ("" : "+g" (suma));
	  asm ("" : "+g" (sumb));
          k++;
          l++;
	}
      if (image[2*i] != suma
	  || image[2*i+1] != sumb)
	abort ();
    }

  return 0;
}

/* { dg-final { scan-tree-dump-times "OUTER LOOP VECTORIZED" 1 "vect" } } */
/* We don't yet support SLP inductions for variable length vectors.  */
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 1 "vect" { xfail vect_variable_length } } } */
