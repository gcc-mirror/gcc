// { dg-do run { target c++17 } }

struct base1 { int b1, b2 = 42; };
struct base2 {
  base2() {
    b3 = 42;
  }
  int b3;
};
struct derived : base1, base2 {
  int d;
};

derived d1{{1, 2}, {}, 4};
derived d2{{}, {}, 4};

#define assert(X) do { if (!(X)) __builtin_abort(); } while(0)
int main()
{
  assert (d1.b1 == 1 && d1.b2 == 2 && d1.b3 == 42 && d1.d == 4);
  assert (d2.b1 == 0 && d2.b2 == 42 && d2.b3 == 42 && d2.d == 4);
}
