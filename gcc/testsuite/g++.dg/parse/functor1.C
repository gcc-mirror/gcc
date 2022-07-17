// PR c++/64679
// { dg-do run }

struct F {
  F(int) { }
  F(int, int) { }
  F operator()(int) const { return *this; }
  F operator()(int, int) const { return *this; }
};

int main()
{
  // Init-declarators.
  int i = 0;
  int (j)(1);
  // None of these is an init-declarator.
  F(i)(1)(2);
  F(i)(1, 2)(3);
  F(i)(1)(2, 3);
  F(i)(2)(3)(4)(5);
  F(i, j)(1)(2)(3)(4)(5)(6);
}
