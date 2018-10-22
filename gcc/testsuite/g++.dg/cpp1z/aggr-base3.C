// { dg-do compile { target c++17 } }

struct derived;
struct base { };
struct derived : base {
  int i;
};

derived d1{1};			// { dg-error "base" }
derived d2{{},1};		// OK

