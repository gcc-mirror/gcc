// PR tree-optimization/123513
// { dg-do compile }
// { dg-options "-Wmismatched-new-delete" }

typedef __SIZE_TYPE__ size_t;

template <size_t N>
class A {};
struct B { B (); };

template <size_t N>
void *operator new (size_t, A <N> &);

template <size_t N>
void operator delete (void *, A <N> &);

void
foo (B *, A <1024> &);

void
bar ()
{
  A <1024> a;
  foo (new (a) B (), a);	// { dg-bogus "called on pointer returned from a mismatched allocation function" }
}
