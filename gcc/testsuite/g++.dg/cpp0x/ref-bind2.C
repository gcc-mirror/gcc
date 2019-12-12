// PR c++/91844 - Implement CWG 2352, Similar types and reference binding.
// { dg-do compile { target c++11 } }

// "const int *" and "int *" are reference-related, and 5.4.4.
// says that in that case, if the reference is an rvalue reference,
// the initializer expression shall not be an lvalue.

int &f (const int *&&);

void
fn (int *p)
{
  const int *&&r = p; // { dg-error "cannot bind rvalue reference" }
  f (p); // { dg-error "cannot bind rvalue reference" }
}
