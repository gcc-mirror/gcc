// { dg-do run { target c++11 } }

struct A;
struct B { operator A (); };
struct A { A (const A &) = default; A () = default; B a; };
A a {B {}};
bool seen;

B::operator A ()
{
  seen = true;
  return A ();
}

int
main ()
{
#if __cplusplus > 201703L
  if (!seen)
    __builtin_abort ();
#else
  if (seen)
    __builtin_abort ();
#endif
}
