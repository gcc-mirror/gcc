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
      for (int j = 0; j < 40; j++)
	{
	  suma += j+i;
	  sumb += j+i;
	}
      image[2*i] = suma;
      image[2*i+1] = sumb;
    }
}

int main ()
{
  check_vect ();

  foo ();

#pragma GCC novector
  for (int i = 0; i < 20; i++)
    {
      double suma = 0;
      double sumb = 0;
      for (int j = 0; j < 40; j++)
	{
	  suma += j+i;
	  sumb += j+i;
	  asm ("" : "+g" (suma));
	  asm ("" : "+g" (sumb));
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
