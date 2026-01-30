/* PR middle-end/123876 */

void
foo ()
{
lab:
#pragma omp simd
  for (int i = 0; i < 4; i++)
    asm goto ("" : : : : lab);			/* { dg-error "invalid branch to/from OpenMP structured block" } */
#pragma omp for
  for (int i = 0; i < 4; i++)
   asm goto ("" : : : : lab);			/* { dg-error "invalid branch to/from OpenMP structured block" } */
#pragma omp parallel
  asm goto ("" : : : : lab);			/* { dg-error "invalid branch to/from OpenMP structured block" } */
}

void
bar ()
{
lab3:
#pragma omp simd
  for (int i = 0; i < 4; i++)
    {
    lab1:
    lab2:
      asm goto ("" : : : : lab1, lab2, lab3);	/* { dg-error "invalid branch to/from OpenMP structured block" } */
    }
#pragma omp for
  for (int i = 0; i < 4; i++)
    {
    lab4:
    lab5:
      asm goto ("" : : : : lab4, lab5, lab3);	/* { dg-error "invalid branch to/from OpenMP structured block" } */
    }
#pragma omp parallel
  {
  lab6:
  lab7:
    asm goto ("" : : : : lab6, lab7, lab3);	/* { dg-error "invalid branch to/from OpenMP structured block" } */
  }
}

void
baz ()
{
#pragma omp simd
  for (int i = 0; i < 4; i++)
    {
    lab1:
    lab2:
      asm goto ("" : : : : lab1, lab2, lab3);
    lab3:;
    }
#pragma omp for
  for (int i = 0; i < 4; i++)
    {
    lab4:
    lab5:
      asm goto ("" : : : : lab4, lab5, lab6);
    lab6:;
    }
#pragma omp parallel
  {
  lab7:
  lab8:
    asm goto ("" : : : : lab7, lab8, lab9);
  lab9:;
  }
}
