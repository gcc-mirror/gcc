// { dg-do compile { target c++14 } }
// PR c++/81574 references to functions are captured by reference.

// 8.1.5.2/10
// For each entity captured by copy, ... an lvalue reference to the
// referenced function type if the entity is a reference to a function

void f (void (&b)())
{
  [=] {  b; } ();
  [=, b(f)] { b; } ();
  [=, b(b)] { b; } ();
}
