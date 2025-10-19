// test that a noexcept function can't throw even if a violation handler throws
// { dg-do run { target c++26 } }
// { dg-additional-options "-fcontracts" }
// { dg-additional-sources "throwing-violation-handler.cc" }

#include <exception>

void f(int x) noexcept pre(x >= 0)
{
}

void g();

bool f_result = true;

void my_term()
{
  try { throw; }
  catch(int) { __builtin_exit(0); }
}


int main()
{
  std::set_terminate (my_term);
  try
  {
    g();
  } catch (...) {
    // We should not get here
    __builtin_abort();
  }
  if (!f_result)
    // We should not get here
    __builtin_abort();
  // We should not get here
  __builtin_abort();
}
