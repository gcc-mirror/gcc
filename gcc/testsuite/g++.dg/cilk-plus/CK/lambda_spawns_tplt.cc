/* { dg-options "-fcilkplus" } */
/* { dg-do run { target i?86-*-* x86_64-*-* arm*-*-* } } */
/* { dg-options "-std=c++11 -fcilkplus -lcilkrts" { target { i?86-*-* x86_64-*-* arm*-*-* } } } */

#define FIRST_NUMBER 5
#define SECOND_NUMBER 3
#define HAVE_IO 0
#if HAVE_IO
#include <stdio.h>
#endif

#include <stdlib.h>

template <class T>
void foo1(T *array, int size)
{
#if HAVE_IO
  for (int ii = 0; ii < size; ii++) 
    printf("%2d\t", (int)array[ii]);
  printf("\n");
  fflush (stdout);
#else
  if (size != 2)
    __builtin_abort ();
  if (array[0] != FIRST_NUMBER)
    __builtin_abort ();
  if (array[1] != SECOND_NUMBER)
    __builtin_abort ();
#endif
}
template <class T>
void foo1_c(const T *array, int size)
{
#if HAVE_IO
  for (int ii = 0; ii < size; ii++) 
    printf("%2d\t", (int)array[ii]);
  printf("\n");
  fflush (stdout);
#else
  if (size != 2)
    __builtin_abort ();
  if (array[0] != FIRST_NUMBER)
    __builtin_abort ();
  if (array[1] != SECOND_NUMBER)
    __builtin_abort ();
#endif
}
template <class T>
int main2 (int argc, char **argv) {
  T A[2] = {FIRST_NUMBER, SECOND_NUMBER};
  int main_size = argc+1; /* We know argc is 1, and so 1+1 = 2.  */
  auto func0 = [=](){ foo1_c(A, 2); };
  _Cilk_spawn func0();
  foo1 (A, 2);
  _Cilk_sync;

  auto func1 = [=](T *Aa){ foo1(Aa, 2); };
  _Cilk_spawn func1 (A);
  foo1 (A, 2);
  _Cilk_sync;

  auto func2 = [=](T *Aa, int size){ foo1(Aa, size); };
  _Cilk_spawn func2 (A, 2);
  foo1 (A, 2);
  _Cilk_sync;

  auto func3 = [=](T *Aa, int size){ int new_size = (size % 2 + 2); 
				       foo1(Aa, size); };
  _Cilk_spawn func3 (A, 2);
  foo1 (A, 2);
  _Cilk_sync;

  auto func4 = [](T *Aa){ foo1(Aa, 2); };
  _Cilk_spawn func4 (A);
  foo1 (A, 2);
  _Cilk_sync;

  auto func5 = [](T *Aa, int size){ foo1(Aa, size); };
  _Cilk_spawn func5 (A, 2);
  foo1 (A, 2);
  _Cilk_sync;

  auto func6 = [&](T *Aa){ foo1(Aa, 2); };
  _Cilk_spawn func6 (A);
  foo1 (A, 2);
  _Cilk_sync;

  auto func7 = [&](T *Aa, int size){ foo1(Aa, size); };
  _Cilk_spawn func7 (A, 2);
  foo1 (A, 2);
  _Cilk_sync;

  auto func8 = [&](){ foo1(A, 2); };
  _Cilk_spawn func8 ();
  foo1 (A, 2);
  _Cilk_sync;

  /* We ignore the first param here and pass in A from the outer fn.  */
  auto func9 = [&](T *Aa, int size){ foo1(A, size); };
  _Cilk_spawn func9 (A, 2);
  foo1 (A, 2);
  _Cilk_sync;

  auto func10 = [=](){ foo1_c(A, main_size); };
  _Cilk_spawn func10 ();
  foo1 (A, 2);
  _Cilk_sync;

  auto func11 = [&](){ foo1(A, main_size); };
  _Cilk_spawn func11 ();
  foo1 (A, 2);
  _Cilk_sync;

  /* We ignore the first & second param here and pass in A from the 
     outer fn.  */
  auto func12 = [&](T *Aa, int size){ foo1(A, main_size); };
  _Cilk_spawn func12 (A, 2);
  foo1 (A, 2);
  _Cilk_sync;

  _Cilk_spawn [&](T *Aa){ foo1(Aa, 2); }(A);
  foo1 (A, 2);
  _Cilk_sync;

  _Cilk_spawn [&](T *Aa, int size){ foo1(Aa, size); }(A, 2);
  foo1 (A, 2);
  _Cilk_sync;

  _Cilk_spawn [=](T *Aa){ foo1(Aa, 2); }(A);
  foo1 (A, 2);
  _Cilk_sync;

  _Cilk_spawn [=](T *Aa, int size){ foo1(Aa, size); }(A, 2);
  foo1 (A, 2);
  _Cilk_sync;

  /* We ignore the first param here.  */
  _Cilk_spawn [=](T *Aa, int size){ foo1_c(A, size); }(A, 2);
  foo1 (A, 2);
  _Cilk_sync;

  /* We ignore the first and second param here.  */
  _Cilk_spawn [=](T *Aa, int size){ foo1_c(A, main_size); }(A, 2);
  foo1 (A, 2);
  _Cilk_sync;

  _Cilk_spawn [&](){ foo1(A, 2); }();
  [&](){ foo1(A, 2); }();
  _Cilk_sync;

  _Cilk_spawn [=](){ foo1_c(A, main_size); }();
  foo1 (A, 2);
  _Cilk_sync;
	
  _Cilk_spawn [&](){ foo1(A, main_size); }();
  [&](){ foo1(A, 2); }();
  _Cilk_sync;

  return 0;
}

int main (void)
{
  int argc = 1;
  char **argv = NULL;
  int x = 1, y = 1, z = 1, q = 1, p = 1;
  x = main2<char>(argc,argv);
  y = main2<short>(argc,argv);
  z = main2<int>(argc,argv);
  p = main2<long>(argc,argv);
  q = main2<long long>(argc,argv);
  return (x+y+z+p+q);
}
