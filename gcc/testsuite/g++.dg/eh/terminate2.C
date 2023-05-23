// PR c++/97720
// { dg-do run }

// Test that there is an active exception when we reach the terminate handler.

#include <exception>
#include <cstdlib>

void bad_guy() throw() {
  try { throw 0; }
  catch (float) { }
  // Don't catch int.
}

void level1() {
  bad_guy();
  throw "dead code";
}

void my_term()
{
  try { throw; }
  catch(...) { std::exit(0); }
}

int main() {
  std::set_terminate (my_term);
  try { level1(); }
  catch (int) { }
}
