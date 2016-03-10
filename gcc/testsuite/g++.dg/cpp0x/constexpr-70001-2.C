// PR c++/70001
// { dg-do run { target c++11 } }

struct B
{
  struct B *a;
  constexpr B () : a (this) { }
};

constexpr int N = 1 << 4;
struct A { B c[N]; } d;

int
main ()
{
  for (int i = 0; i < N; ++i)
    if (d.c[i].a != &d.c[i])
      __builtin_abort ();
}
