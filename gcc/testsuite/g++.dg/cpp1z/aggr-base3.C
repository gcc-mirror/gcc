// { dg-options -std=c++1z }

struct derived;
struct base { };
struct derived : base {
  int i;
};

derived d1{1};			// { dg-error "base" }
derived d2{{},1};		// OK

