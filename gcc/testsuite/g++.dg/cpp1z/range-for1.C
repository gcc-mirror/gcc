// P0184R0: Generalizing the Range-Based For Loop
// { dg-options "-std=c++1z" }

struct A {
  int ar[4];
  int *begin() { return ar; }
  struct end_t {
    int *p;
    friend bool operator!= (int *p, end_t e) { return p != e.p; }
  };
  end_t end() { return { &ar[4] }; }
};

int main()
{
  A a { 1, 2, 3, 4 };
  int i = 1;
  for (auto x: a)
    if (x != i++)
      __builtin_abort ();
  if (i != 5)
    __builtin_abort ();
}
