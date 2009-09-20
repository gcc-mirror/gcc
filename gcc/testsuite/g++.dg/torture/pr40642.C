// { dg-do compile }
// { dg-options "-fprofile-generate" }

// GCC used to ICE with some EH edge missing.

inline void* operator new(__SIZE_TYPE__, void* p) throw() { return p; }
inline void operator delete (void*, void*) throw() { }

template<typename T> void foo(void* p, T t)
{
  new(p) T(t);
}

void bar();

template<typename T> struct A
{
  T* p;

  A() { try { foo(p, T()); } catch(...) {} }

  A(const A&) { try { bar(); } catch(...) { throw; } }
};

A<A<int> > a;

