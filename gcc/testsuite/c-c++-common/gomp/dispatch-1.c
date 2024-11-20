#include <stdint.h>

int f0 (int, long, double);
void f2 (void);
int f3 (void);
void (*f4) (void);
void f5 (int*, int);

int *pp (void);
int dd (void);

void f1 (void)
{
  int a, c;
  long b;
  double x;
  struct {int a; float b; short c;} s, *sp;
  int arr[3];

#pragma omp dispatch
  c = f0 (a, b, x);
#pragma omp dispatch
  x = f0 (a * 4, 2 - b, x * x);
#pragma omp dispatch
  s.a = f0 (a, sp->c, x);
#pragma omp dispatch
  sp->c = f0 (s.a - 2, b / 3, x * 5);
#pragma omp dispatch
  arr[0] = f0 (arr[1], !b, arr[2]);
#pragma omp dispatch
  (*sp).c = f0 (s.a, b, x);
#pragma omp dispatch
  sp->b = f0 (s.a++, b % 4, --x);
#pragma omp dispatch
  f0 (f3(), b, s.b);
#pragma omp dispatch
  f2 ();
#pragma omp dispatch
  f4 ();
#pragma omp dispatch
  f5 (pp(), pp()[dd()]);
  
#pragma omp dispatch nocontext(sp->a * x + arr[2])
  f2 ();
#pragma omp dispatch nocontext(arr - (intptr_t)(x / s.b))
  f2 ();
#pragma omp dispatch nocontext(x == s.c || b != c)
  f2 ();
#pragma omp dispatch novariants(b << sp->c)
  f2 ();
#pragma omp dispatch novariants(!arr | s.a)
  f2 ();
#pragma omp dispatch novariants(s.c ? f3() : a & c)
  f2 ();
#pragma omp dispatch nowait
  f2 ();
#pragma omp dispatch device(-25373654)
  f2 ();
#pragma omp dispatch device(b * (int)(x - sp->b))
  f2 ();
#pragma omp dispatch is_device_ptr(arr)
  f2 ();
#pragma omp dispatch is_device_ptr(sp)
  f2 ();
#pragma omp dispatch depend(inout: sp)
  f2 ();
#pragma omp dispatch depend(inoutset: arr[:2])
  f2 ();
#pragma omp dispatch depend(out: arr)
  f2 ();
}
