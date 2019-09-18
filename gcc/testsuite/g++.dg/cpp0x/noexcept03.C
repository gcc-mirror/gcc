// Runtime test for noexcept-specification.
// { dg-options "-Wnoexcept" }
// { dg-do run { target { nonpic || pie_enabled } } }
// { dg-require-effective-target c++11 }

#include <exception>
#include <cstdlib>

void my_terminate ()
{
  std::exit (0);
}

void my_unexpected ()
{
  throw;
}

void g() { throw 1; }
void (*p)() = g;
void f () noexcept (false)
{
  p();
}

template <class T>
void f(T) noexcept (noexcept (T())) // { dg-warning "false" }
{
  p();
}

template <class T>
void f2(T a) noexcept (noexcept (f (a)))
{
  f(a);
}

struct A { A() { } };		// { dg-warning "does not throw" }

int main()
{
  // noexcept(false) allows throw.
  try { f(); } catch (int) { }
  // noexcept(noexcept(A())) == noexcept(false).
  try { f(A()); } catch (int) { }
  try { f2(A()); } catch (int) { }

  std::set_terminate (my_terminate);
  // noexcept(noexcept(int())) == noexcept(true).
  try { f2(1); } catch (...) { }
  return 1;
}
