// { dg-do assemble  }
union A {
 struct B { };
 A::B b;			// { dg-bogus "" } 
};
