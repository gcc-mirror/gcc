// PR c++/91304 - prefix attributes ignored in condition.
// { dg-do compile { target c++11 } }
// { dg-additional-options "-Wall -Wextra" }

int f();

void g()
{   
  if ([[maybe_unused]] int i = f()) { }
  if ([[deprecated]] int i = f()) { i = 10; } // { dg-warning ".i. is deprecated" }
  if (int i [[maybe_unused]] = f()) { }
  if (int i [[deprecated]] = f()) { i = 10; } // { dg-warning ".i. is deprecated" }
}
