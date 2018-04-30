// PR c++/81640
// { dg-do compile }
// { dg-options "-Wshadow=compatible-local" }

struct A {};
struct B { operator bool () const { return true; } };

template <typename T>
void
foo ()
{
  T d, e;
  if (e)
    A d;
}

void
bar ()
{
  foo <B> ();
}
