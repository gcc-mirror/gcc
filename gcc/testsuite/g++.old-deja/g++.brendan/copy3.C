// GROUPS passed copy-ctors
/*

If I compile it with cfront (AT&T C++ Translator 2.00.02 08/25/89) and run it
I get:

	A::A()
	A::A(const A&)
	B::Bar()
	A::~A()
	A::~A()

If I compile it with g++ (gcc version 2.2.2) and run it I get:

	A::A()
	B::Bar()
	A::~A()
	A::~A()

*/
extern "C" void printf (char *, ...);
extern "C" void exit (int);

int count = 0;

void
die (int x)
{
  if (x != ++count)
    {
      printf ("FAIL\n");
      exit (1);
    }
}
  

class A {
public:
  A() { die (1); }
  A(const A&) { die (2); }
  ~A() { count++; if (count != 4 && count != 5) die (-1); }
};

class B : public A {
public:
  void Bar() { die (3); }
};

void Foo(B b) { b.Bar(); }

int
main()
{
  B b;
  Foo(b);

  printf ("PASS\n");
}
