// PR c++/94955
// { dg-do compile { target c++11 } }

struct S {
  static constexpr char foo() { return 10; }
};

short int
fn (short int e)
{
  return e >> S::foo();
}
