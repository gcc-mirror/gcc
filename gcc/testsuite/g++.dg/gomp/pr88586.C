// PR ipa/88586
// { dg-do compile { target lto } }
// { dg-options "-fopenmp -flto" }

extern "C" int omp_get_cancellation ();
extern "C" int omp_get_thread_num ();
extern "C" void abort ();

struct A { A (); ~A (); A (const A &); static int cnt1, cnt2, cnt3; int a; };
int A::cnt1;
int A::cnt2;
int A::cnt3;
A::A () : a (0)
{
  #pragma omp atomic
  cnt1++;
}
A::A (const A &x) : a (x.a)
{
  #pragma omp atomic
  cnt2++;
}
A::~A ()
{
  #pragma omp atomic
  cnt3++;
}
#pragma omp declare reduction (+: A: omp_out.a += omp_in.a)

void
foo (int x)
{
  A a, b[2];
  int d = 1;
  long int e[2] = { 1L, 1L };
  int c = 0;
  #pragma omp parallel
  {
    if (x && omp_get_thread_num () == 0)
      {
	for (int i = 0; i < 10000000; ++i)
	  asm volatile ("");
	c = 1;
	#pragma omp cancel parallel
      }
    #pragma omp for reduction (task, +: a, b) reduction (task, *: d, e)
    for (int i = 0; i < 64; i++)
      #pragma omp task in_reduction (+: a, b) in_reduction (*: d, e)
      {
	a.a++;
	b[0].a += 2;
	b[1].a += 3;
	d *= ((i & 7) == 0) + 1;
	e[0] *= ((i & 7) == 3) + 1;
	e[1] *= ((i & 3) == 2) + 1;
      }
    if (x && omp_get_cancellation ())
      abort ();
  }
  if (!c)
    {
      if (a.a != 64 || b[0].a != 128 || b[1].a != 192)
	abort ();
      if (d != 256 || e[0] != 256L || e[1] != 65536L)
	abort ();
    }
}

int
main ()
{
  int c1 = A::cnt1, c2 = A::cnt2, c3 = A::cnt3;
  volatile int zero = 0;
  foo (zero);
  if (A::cnt1 + A::cnt2 - c1 - c2 != A::cnt3 - c3)
    abort ();
}
