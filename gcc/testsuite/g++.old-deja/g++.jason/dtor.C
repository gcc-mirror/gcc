struct A {
  ~A();
};

struct B {
  ~B();
};

main()
{
  A a;
  a.~B();			// ERROR - wrong name
}
