// PR tree-optimization/83283
// { dg-do run }
// { dg-additional-options "-std=c++11" }

enum E : unsigned char { X = 0, Y = 1 };

void __attribute__((noinline))
foo (E *v, int size)
{
  for (int i = 0; i < size; ++i)
    {
      const bool b = (v[i] == E::Y);
      v[i] = static_cast<E>(static_cast<unsigned char>(b));
    }
}

int
main ()
{
  constexpr int items = 32;
  E vals[items] = {X};
  vals[3] = Y;
  foo (vals, items);
  if (vals[3] != 1)
    __builtin_abort ();
}
