// PR c++/18674

template <typename I>
static void g() {
  enum I::t a; // { dg-error "" }
  (void) a;
}

struct B {
  typedef int t;
};

void h()
{
  g<B>();
}
