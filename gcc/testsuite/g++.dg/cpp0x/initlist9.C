// PR c++/37860
// { dg-options "-std=c++0x" }

struct b
{
  bool t;

  b() = default;
  ~b() = default;
  b& operator=(const b&) = delete;
  b(const b&) = delete;		// { dg-error "deleted" }

  b(bool _t): t (_t) { }
};

int main()
{
  // copy list initialization
  b tst1 = { false };

  // copy initialization.
  b tst2 = false;		// { dg-error "used here" }

  // direct list initialization
  b tst3 { false };

  // default initialization
  b tst4;
}
