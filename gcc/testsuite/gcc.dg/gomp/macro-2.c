// { dg-do compile }

#define p	parallel
#define s(x)	shared(x##1, x##2)
#define d(x)	default(x)

void bar(int, int, int, int);
void foo(void)
{
  int a1, a2, b1, b2;

  #pragma omp p s(a) s(b) d(none)
    bar(a1, a2, b1, b2);
}
