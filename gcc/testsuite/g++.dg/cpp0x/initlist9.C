// PR c++/37860
// { dg-do compile { target c++11 } }

struct b
{
  bool t;

  b() = default;
  ~b() = default;
  b& operator=(const b&) = delete;
  b(const b&) = delete;		// { dg-message "declared" "" { target c++14_down } }

  b(bool _t): t (_t) { }
};

int main()
{
  // copy list initialization
  b tst1 = { false };

  // copy initialization.
  b tst2 = false;		// { dg-error "use" "" { target c++14_down } }

  // direct list initialization
  b tst3 { false };

  // default initialization
  b tst4;
}
