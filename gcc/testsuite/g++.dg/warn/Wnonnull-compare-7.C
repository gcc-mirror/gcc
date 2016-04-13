// PR c++/69922
// { dg-do compile }
// { dg-options "-Wnonnull-compare" }

struct S { virtual ~S (); };
struct T { virtual ~T (); };
bool b, c;
S *p;
T *q, *r;

S::~S ()
{
  delete (b ? this : p);		// { dg-bogus "nonnull argument" }
}

T::~T ()
{
  delete (b ? (c ? this : q) : r);	// { dg-bogus "nonnull argument" }
}
