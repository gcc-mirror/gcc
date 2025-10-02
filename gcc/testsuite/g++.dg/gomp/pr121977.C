// PR c++/121977
// { dg-do compile }
// { dg-additional-options "-ftrivial-auto-var-init=zero" }

struct T { T () {}; virtual ~T () {}; int t; };
struct S : virtual public T { int a; void foo (); };

void
S::foo ()
{
#pragma omp parallel
  {
    #pragma omp taskloop firstprivate (a, t) lastprivate (t)
    for (int i = 0; i < a; i++)
      t++;
  }
}
