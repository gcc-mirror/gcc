// PR c++/114634
// { dg-do compile }

template <int N>
struct A
{
  enum { e __attribute__ ((aligned (16))) };	// { dg-error "alignment may not be specified for 'e'" }
};
