// PR c++/6794
// Test whether __PRETTY_FUNCTION__ works in templates, functions and
// in initializers at global scope
// { dg-do run }
// { dg-options "" }

extern "C" void __assert_fail (const char *, const char *,
			       unsigned int, const char *)
  throw() __attribute__((noreturn));
extern "C" void abort (void);
extern "C" void exit (int);

#define str(expr) #expr
#define assert(expr)						\
  ((expr) ? 0 : (__assert_fail (str(expr), __FILE__, __LINE__,	\
				__PRETTY_FUNCTION__), 0))

int __attribute__((noinline))
foo (void)
{
  return 1;
}

template<class T> int
bar (T)
{
  return (assert (foo ()), 1);
}

template<> int
bar<int> (int)
{
  return (assert (foo ()), 2);
}

int a = (assert (foo ()), 1);
int b = (assert (foo ()), 2);

int
main ()
{
  double c = 1.0;
  unsigned char *d = 0;
  int e = (assert (foo ()), 3);

  bar (c);
  bar (d);
  bar (e);
}

namespace N
{
  int f = (assert (foo ()), 4);
}

void __attribute__((noinline))
__assert_fail (const char *cond, const char *file, unsigned int line,
	       const char *pretty) throw ()
{
  abort ();
}
