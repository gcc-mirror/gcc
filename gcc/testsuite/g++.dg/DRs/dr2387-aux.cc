// DR 2387

template <int N>
extern const int a;
template <int N>
static const int b = N;
template <int N>
extern const int c;
template <int N>
extern const volatile int d;
template <int N>
extern const int e;
extern const int *pa, *pb, *pc, *pe;
extern const volatile int *pd;

int
main ()
{
  if (pa != &a <42>
      || pb == &b <42>
      || pc != &c <42>
      || pd != &d <42>
      || pe != &e <43>)
    __builtin_abort ();
}
