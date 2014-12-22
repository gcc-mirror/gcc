// Test for C++14 sized deallocation.  The operators delete defined below
// should be called only in C++14 mode and above.
// { dg-do run }

extern "C" void abort();
typedef __SIZE_TYPE__ size_t;
#include <new>

bool called;
void operator delete[] (void *p, size_t s) throw()
{
  called = true;
  operator delete[] (p);
}

void operator delete (void *p, size_t s) throw()
{
  called = true;
  operator delete (p);
}

void operator delete[] (void *p, size_t s, const std::nothrow_t &) throw()
{
  called = true;
  operator delete[] (p);
}

void operator delete (void *p, size_t s, const std::nothrow_t &) throw()
{
  called = true;
  operator delete (p);
}

struct A { ~A(){} };

struct B { };

struct C;

struct D { ~D(){}; D() { throw 1; } };

int main()
{
  /* * If the type is complete and if, for the second alternative (delete
     array) only, the operand is a pointer to a class type with a
     non-trivial destructor or a (possibly multi-dimensional) array
     thereof, the function with two parameters is selected.

     * Otherwise, it is unspecified which of the two deallocation functions
     is selected. */
  delete new int;
  if (called != (__cplusplus >= 201402L)) abort(); called = false;

  delete new A;
  if (called != (__cplusplus >= 201402L)) abort(); called = false;

  delete[] new A[2];
  if (called != (__cplusplus >= 201402L)) abort(); called = false;

  delete new B;
  if (called != (__cplusplus >= 201402L)) abort(); called = false;

  /* N3778 added the sized placement deallocation functions, but the core
     language rules don't provide any way they would be called.  */
  try { new (std::nothrow) D; } catch (int) {}
  if (called) abort();

  try { new (std::nothrow) D[2]; } catch (int) {}
  if (called) abort();

  /* Make sure we don't try to use the size of an array that doesn't have a
     cookie.  */
  delete[] new B[2];
  if (called) abort();
}
