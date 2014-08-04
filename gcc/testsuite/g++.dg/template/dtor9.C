// PR c++/60347

struct A;

template <class T>
struct B
{
  T* p;
  virtual ~B() { p->~T(); }
};

struct C: B<A> { };
