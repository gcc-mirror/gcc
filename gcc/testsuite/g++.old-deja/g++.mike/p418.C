// Build don't link:
// prms-id: 418

class Base {
public:
	int foo;
};

class Derived : public Base {
public:
	int bar;
};

void func(Base&);		// ERROR - referenced by error below

void func2(const Derived& d) {
	func(d);		// ERROR - should be error because of const
}
