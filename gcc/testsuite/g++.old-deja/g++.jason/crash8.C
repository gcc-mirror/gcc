struct A {
  A();
  A(A);				// ERROR - copy ctor must take reference
};
main()
{
  A a;
  A b(a);			// causes compiler segfault
}
