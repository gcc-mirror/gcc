// Testcase for tricky synthesized op= in complex inheritance situation.
// See discussion in g++int.texi.

// execution test - XFAIL *-*-*

int count = 0;
extern "C" int printf (const char *, ...);

class A {
 public:
  A& operator = (const A&) { count++; return *this; }
};

class B: virtual private A { };
class C: virtual public A { };
class D: public B, public C { };

int main()
{
  D a, b;
  a = b;
  printf ("%d\n",count);
  if (count != 1)
    return 1;
  return 0;
}
