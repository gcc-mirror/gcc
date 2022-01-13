// PR c++/50866, adjusted
// { dg-do run }

#if __cplusplus > 201100L
#define THROWING noexcept(false)
#else
#define THROWING
#endif

extern "C" void abort ();

#ifdef DEBUG
  extern "C" int printf (const char *, ...);
  #define dump(X,Y) printf(X,Y)
#define abort() printf("wrong\n");

#else
  #define dump(X,Y)
#endif

int a, b;
int d;
struct A {
  int n;
  A() { n = ++a; dump("A%d\n",a); }
  A(const A&);
  ~A() THROWING {
    dump("~A%d\n",n);
    --a;
    if (d == 1 ? a == 0 : (b == d && a == 1))
      {
	dump ("~A%d throwing\n", n);
	throw (short)b;
      }
  }
};
int t;
struct B {
  int n;
  B(const A& = A())
  {
    if (b == t)
      {
	dump ("B%d throwing\n", b+1);
	throw b;
      }
    n = ++b;
    dump("B%d\n",b);

    /* The first B has an explicit initializer, so its A lives for the
       full-expression.  The second B does not, so its A should be destroyed
       before we construct the third B.  */
    if (a != 2) abort ();
  }
  B(const char *, const A& = A())
  {
    if (b == t)
      {
	dump ("B%d throwing\n", b+1);
	throw b;
      }
    n = ++b;
    dump("B%d\n",b);
    if (a != b) abort ();
  }
  B(const B&);
  ~B()
  {
    dump("~B%d\n",n);
    --b;
  }
};
struct C {
  B bs[3];
};
void f()
{
  a = b = 0;
  try
    {
      C c = { "x" };
      if (a != 0) abort ();
      if (b != 3) abort ();
    }
  catch (int i) { }
  catch (short s) { }
  if (a != 0) abort ();
  if (b != 0) abort ();
  dump ("\n", 0);
}

int main()
{
  for (t = 0; t <= 3; ++t)
    f();
  for (d = 1; d <= 3; ++d)
    f();
}
