// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin: PR c++/40684
// { dg-do compile { target c++11 } }

struct A
{
};

template <typename S, typename T, typename U,
	  typename S::v = &S::v::s> // { dg-error "is not a" }
typename S::A
foo (S c, T t, U u)
{
}

struct B
{
  struct C
  {
    template <typename U>
    C (U t)
    {
      A a;
      A b = foo (this, a, t); // { dg-error "no matching function" }
    }
  } c;
  B () : c (A ())
  {
  }
};

int
main ()
{
  B f;
}

