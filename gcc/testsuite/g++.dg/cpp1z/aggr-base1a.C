// { dg-do compile { target { c++11 && c++14_down } } }

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

derived d1{{1, 2}, {}, 4};	// { dg-error "" }
derived d2{{}, {}, 4};		// { dg-error "" }
