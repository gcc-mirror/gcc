// PR c++/60347
// { dg-options "-fuse-all-virtuals" }

struct A;

template <class T>
struct B
{
  T* p;
  virtual ~B() { p->~T(); }	// { dg-error "incomplete" }
};

struct C: B<A> { };
