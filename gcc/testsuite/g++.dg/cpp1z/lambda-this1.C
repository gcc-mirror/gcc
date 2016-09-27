// P0018R3 - C++17 lambda capture of *this
// { dg-do compile { target c++11 } }

struct A {
  int a;
  void foo () {
    int v = 4;
    auto b = [*this, this] {};		// { dg-error "already captured 'this'" }
					// { dg-error "'*this' capture only available with" "" { target c++14_down } .-1 }
    auto c = [this, *this] {};		// { dg-error "already captured 'this'" }
					// { dg-error "'*this' capture only available with" "" { target c++14_down } .-1 }
    auto d = [this] { return a; };
    auto e = [*this] { return a; };	// { dg-error "'*this' capture only available with" "" { target c++14_down } }
    auto f = [this] { a++; };
    auto g = [*this] { a++; };		// { dg-error "in read-only object" }
					// { dg-error "'*this' capture only available with" "" { target c++14_down } .-1 }
    auto h = [*this] () mutable { a++; };// { dg-error "'*this' capture only available with" "" { target c++14_down } }
    auto i = [=] { return a; };
    auto j = [&] { return a; };
    auto k = [=, this] { return a; };// { dg-error "explicit by-copy capture of 'this' redundant with by-copy capture default" }
    auto l = [&, this] { return a; };
    auto m = [=, *this] { return a; };// { dg-error "'*this' capture only available with" "" { target c++14_down } }
    auto n = [&, *this] { return a; };// { dg-error "'*this' capture only available with" "" { target c++14_down } }
    auto o = [*this, &v] { return a + v; };// { dg-error "'*this' capture only available with" "" { target c++14_down } }
    auto p = [*this] { this = 0; };	// { dg-error "lvalue required as left operand of assignment" }
					// { dg-error "'*this' capture only available with" "" { target c++14_down } .-1 }
  }
};
struct B {
  double b;
  B () : b (.007) {}
  double foo () {
    return [this]{ return [*this] { return b; }; }()();	// { dg-error "'*this' capture only available with" "" { target c++14_down } }
  }
  double bar () {
    auto c = []{ return [*this] { return b; }; };	// { dg-error "'this' was not captured for this lambda function" }
  }							// { dg-error "invalid use of non-static data member 'B::b'" "" { target *-*-* } .-1 }
};							// { dg-error "'*this' capture only available with" "" { target c++14_down } .-2 }
struct C {
  int c;
  C (const C &) = delete;
  void bar () const;
  void foo () {
    auto d = [this] { return c; };
    auto e = [*this] { return c; };	// { dg-error "use of deleted function" }
					// { dg-error "'*this' capture only available with" "" { target c++14_down } .-1 }
    auto f = [=] { return c; };
    auto g = [&] { return c; };
    auto h = [this] { bar (); };
    auto i = [*this] { bar (); };	// { dg-error "use of deleted function" }
					// { dg-error "'*this' capture only available with" "" { target c++14_down } .-1 }
  }
};
struct D {
  int d;
  ~D () = delete;
  void bar () const;
  void foo () {
    auto e = [this] { return d; };
    auto f = [*this] { return d; };	// { dg-error "use of deleted function" }
					// { dg-error "'*this' capture only available with" "" { target c++14_down } .-1 }
    auto g = [=] { return d; };
    auto h = [&] { return d; };
    auto i = [this] { bar (); };
    auto j = [*this] { bar (); };	// { dg-error "use of deleted function" }
					// { dg-error "'*this' capture only available with" "" { target c++14_down } .-1 }
  }
};
