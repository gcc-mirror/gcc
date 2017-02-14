// PR c++/68703

template <int N>
struct D {
  int v __attribute__((vector_size (N * sizeof (int))));
  int f1 () { return this->v[N-1]; }
  int f2 () { return v[N-1]; }
};

int
main ()
{
  D<4> a = { { 0, 1, 2, 3 } };
  D<8> b = { { 0, 1, 2, 3, 4, 5, 6, 7 } };
  if (a.f1 () != 3 || a.f2 () != 3
      || b.f1 () != 7 || b.f2 () != 7)
    __builtin_abort ();
}
