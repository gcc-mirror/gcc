// PR c++/117158 - Similar to eval-order7.C, only with templates.
// { dg-do run { target c++11 } }
// { dg-options "-fstrong-eval-order" }

int a[4] = { 1, 2, 3, 4 };
int b[4] = { 5, 6, 7, 8 };

struct Base {
  int *intarray;
};

template <typename T>
struct Sub : public Base {
  int Get(int i) {
    Base::intarray = a;
    int r = Base::intarray[(Base::intarray = b, i)];
    if (Base::intarray != b)
      __builtin_abort ();
    return r;
  }
};

int
main ()
{
  Sub<int> s;
  if (s.Get (3) != 4)
    __builtin_abort ();
}
