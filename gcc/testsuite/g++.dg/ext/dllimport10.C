// PR c++/5287, c++/11021
// Inherit a virtual method from a dllimport'd base class.

// { dg-do compile { target i?86-*-cygwin* i?86-*-mingw* x86_64-*-mingw* } }

struct __attribute__((dllimport)) A
{
  virtual void vfunc(void);
};

struct B : public A
{
};


B aB;
