/* { dg-do compile } */

/* Check that various cases of invalid references to variables bound
   in an intervening code scope are diagnosed and do not ICE.  This test
   is expected to produce errors.  */

template<typename T>
extern void foo (T, T);

template<typename T>
void f1 (void)
{
#pragma omp for collapse (2)
  for (T i = 0; i < 64; i++)
    {
      T v = (i + 4) * 2;
      for (T j = v; j < 64; j++)  /* { dg-error "initializer is bound in intervening code" }  */
	foo (i, j);
    }
}

template<typename T>
void f2 (void)
{
#pragma omp for collapse (2)
  for (T i = 0; i < 64; i++)
    {
      T v = (i + 4) * 2;
      for (T j = 0; j < v; j++)  /* { dg-error "end test is bound in intervening code" }  */
	foo (i, j);
    }
}

template<typename T>
void f3 (void)
{
#pragma omp for collapse (2)
  for (T i = 0; i < 64; i++)
    {
      T v = (i + 4) * 2;
      for (T j = 0; j < 64; j = j + v)  /* { dg-error "increment expression is bound in intervening code" }  */
	foo (i, j);
    }
}

template<typename T>
void f4 (void)
{
#pragma omp for collapse (2)
  for (T i = 0; i < 64; i++)
    {
      T v = 8;
      for (T j = v; j < 64; j++)  /* { dg-error "initializer is bound in intervening code" }  */
	foo (i, j);
    }
}

template<typename T>
void f5 (void)
{
#pragma omp for collapse (2)
  for (T i = 0; i < 64; i++)
    {
      T j;
      for (j = 0; j < 64; j++)  /* { dg-error "loop variable is bound in intervening code" }  */
	foo (i, j);
    }
}

template<typename T>
void f6 (void)
{
#pragma omp for collapse (2)
  for (T i = 0; i < 64; i++)
    {
      T j;
      {
	T v = 8;
	for (j = v; j < 64; j++)    /* { dg-error "loop variable is bound in intervening code" }  */
	  /* { dg-error "initializer is bound in intervening code" "" { target *-*-* } .-1 } */
	  foo (i, j);
      }
    }
}

int main()
{
  f1<int> ();
  f2<int> ();
  f3<int> ();
  f4<int> ();
  f5<int> ();
  f6<int> ();
}
