// PR c++/6316
// This testcase ICEd because when deferred bar()::F::F() was being
// expanded, containing bar() was still deferred and had DECL_EXTERNAL set
// (and DECL_NOT_REALLY_EXTERN too).
// { dg-do compile }
// { dg-options "-O3" }

struct A { ~A() throw() {} };
template<typename T, typename U> struct B { U a; B(const T *); };
typedef B<char, A> C;
struct D { D(); };
struct E { virtual ~E(); };

E *bar ();

void
foo ()
{
  E *a = bar ();
}

extern char *z [];

E *
bar ()
{
  struct F : public E
  {
    F ()
    {
      for (int i = 0; i < 2; i++)
	C e = z[i];
    }
    D x, y;
  };
  return new F ();
}

int
main ()
{
  foo ();
}
