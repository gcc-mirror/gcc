// PRMS Id: 4066
// Bug: g++ doesn't notice the const on reference returns.

struct B {
  int foo() { return 1; }
  int foo() const { return 0; }
};

B b_;
const B &b () { return b_; }

int main()
{
  return b().foo();
}
