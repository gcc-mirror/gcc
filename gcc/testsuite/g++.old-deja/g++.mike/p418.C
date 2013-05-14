// { dg-do assemble  }
// prms-id: 418

class Base {
public:
	int foo;
};

class Derived : public Base {
public:
	int bar;
};

void func(Base&);		// { dg-message "" } referenced by error below

void func2(const Derived& d) {
	func(d);		// { dg-error "" } should be error because of const
}
