// { dg-do run  }
// Testcase for tricky synthesized op= in complex inheritance situation.
// This used to test whether the virtual base was copy-assigned only once.
// That feature is not required by ISO C++, so the test now only checks
// whether the vbase is assigned at all.

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
  if (count == 0)
    return 1;
  return 0;
}
