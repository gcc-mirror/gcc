/* { dg-do compile } */

/* Check that various cases of invalid references to variables bound
   in an intervening code scope are diagnosed and do not ICE.  This test
   is expected to produce errors.  */

extern void foo (int, int);

void f1 (void)
{
#pragma omp for collapse (2)
  for (int i = 0; i < 64; i++)
    {
      int v = (i + 4) * 2;
      for (int j = v; j < 64; j++)  /* { dg-error "initializer is bound in intervening code" }  */
	foo (i, j);
    }
}

void f2 (void)
{
#pragma omp for collapse (2)
  for (int i = 0; i < 64; i++)
    {
      int v = (i + 4) * 2;
      for (int j = 0; j < v; j++)  /* { dg-error "end test is bound in intervening code" }  */
	foo (i, j);
    }
}

void f3 (void)
{
#pragma omp for collapse (2)
  for (int i = 0; i < 64; i++)
    {
      int v = (i + 4) * 2;
      for (int j = 0; j < 64; j = j + v)  /* { dg-error "increment expression is bound in intervening code" }  */
	foo (i, j);
    }
}

void f4 (void)
{
#pragma omp for collapse (2)
  for (int i = 0; i < 64; i++)
    {
      int v = 8;
      for (int j = v; j < 64; j++)  /* { dg-error "initializer is bound in intervening code" }  */
	foo (i, j);
    }
}

void f5 (void)
{
#pragma omp for collapse (2)
  for (int i = 0; i < 64; i++)
    {
      int j;
      for (j = 0; j < 64; j++)  /* { dg-error "loop variable is bound in intervening code" }  */
	foo (i, j);
    }
}

void f6 (void)
{
#pragma omp for collapse (2)
  for (int i = 0; i < 64; i++)
    {
      int j;
      {
	int v = 8;
	for (j = v; j < 64; j++)    /* { dg-error "loop variable is bound in intervening code" }  */
	  /* { dg-error "initializer is bound in intervening code" "" { target *-*-* } .-1 } */
	  foo (i, j);
      }
    }
}
