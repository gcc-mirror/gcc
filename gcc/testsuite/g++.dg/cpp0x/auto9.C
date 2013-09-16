// PR c++/37962
// Negative test for auto
// { dg-do compile }
// { dg-options "-std=c++0x" }

#include <typeinfo>
#include <stdarg.h>
#include <stddef.h>

int i = *(auto *) 0;				// { dg-error "auto" }
struct A *p = (auto *) 0;			// { dg-error "auto" }
int *q = static_cast <auto *>(0);		// { dg-error "auto" }
const int *r = const_cast <auto *>(q);		// { dg-error "auto" }
const std::type_info &t1 = typeid (auto);	// { dg-error "auto" }
const std::type_info &t2 = typeid (auto *);	// { dg-error "auto" }

struct A
{
  operator auto ();				// { dg-error "auto" }
  operator auto *();				// { dg-error "auto" }
};

struct A2
{
  operator auto () -> int;			// { dg-error "invalid use of" }
  operator auto *() -> int;			// { dg-error "auto" }
};

template <typename> struct B
{
  enum { e };
};

template <typename T> struct C
{
  C () : i () {}
  int i;
};

bool d = (auto (A::*)()) 0;			// { dg-error "auto" }

void
foo ()
{
  (auto) { 0 };					// { dg-error "auto" }
  C<int> c;
  dynamic_cast<auto> (c);			// { dg-error "auto" }
  reinterpret_cast<auto> (c);			// { dg-error "auto" }
  int i = auto (0);				// { dg-error "auto" }
  auto p1 = new (auto);				// { dg-error "auto" }
  auto p2 = new (auto) (42);			// { dg-error "invalid use of|deduce" }
  offsetof (auto, fld);				// { dg-error "auto" }
  offsetof (auto *, fld);			// { dg-error "auto" }
  sizeof (auto);				// { dg-error "auto" }
  sizeof (auto *);				// { dg-error "auto" }
}

void
foo2 (void)
{
  __alignof__ (auto);				// { dg-error "auto" }
  __alignof__ (auto *);				// { dg-error "auto" }
  __typeof__ (auto) v1;				// { dg-error "auto" }
  __typeof__ (auto *) v2;			// { dg-error "auto" }
  __is_class (auto);				// { dg-error "auto|expected" }
  __is_pod (auto *);				// { dg-error "auto|expected" }
  __is_base_of (int, auto);			// { dg-error "auto|expected" }
  __is_base_of (auto, int);			// { dg-error "auto|expected" }
  __is_base_of (auto, auto *);			// { dg-error "auto|expected" }
}

B<auto> b;					// { dg-error "auto|invalid" }
C<auto> c;					// { dg-error "auto|invalid" }
C<auto *> c2;					// { dg-error "auto|invalid" }

enum : auto { EE = 0 };				// { dg-error "must be an integral type" }
enum struct D : auto * { FF = 0 };		// { dg-error "must be an integral type|declar|expected" }

void
bar ()
{
  try { } catch (auto i) { }			// { dg-error "parameter declared" }
  try { } catch (auto) { }			// { dg-error "parameter declared" }
  try { } catch (auto *i) { }			// { dg-error "parameter declared" }
  try { } catch (auto *) { }			// { dg-error "parameter declared" }
}

void
baz (int i, ...)
{
  va_list ap;
  va_start (ap, i);
  va_arg (ap, auto);				// { dg-error "invalid use of" }
  va_arg (ap, auto *);				// { dg-error "invalid use of|expected" }
  va_arg (ap, auto &);				// { dg-error "invalid use of|expected" }
  va_end (ap);
}

template <typename T = auto> struct E {};	// { dg-error "invalid use of" }
template <class T = auto *> struct F {};	// { dg-error "invalid use of|expected" }

auto fnlate () -> auto;				// { dg-error "invalid use of" }
auto fnlate2 () -> auto *;			// { dg-error "invalid use of|expected" }

void
badthrow () throw (auto)			// { dg-error "invalid use of" }
{
}

void
badthrow2 () throw (auto &)			// { dg-error "invalid use of|expected" }
{
}

template <auto V = 4> struct G {};		// { dg-error "auto" }

template <typename T> struct H { H (); ~H (); };
H<auto> h;					// { dg-error "invalid" }

void qq (auto);			// { dg-warning "auto" }
void qr (auto*);		// { dg-warning "auto" }

// PR c++/46145
typedef auto autot;		// { dg-error "auto" }
