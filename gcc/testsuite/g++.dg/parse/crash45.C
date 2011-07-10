// PR c++/37348
// { dg-do compile }

struct A
{
  template <class> int f (B);	// { dg-error "was not declared in this scope|cannot be a member template|has not been declared" }
};
