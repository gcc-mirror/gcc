// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin: PR c++/40684
// { dg-options "-std=c++0x" }

struct A
{
};

template <typename S, typename T, typename U, typename S::v = &S::v::s>
typename S::A
foo (S c, T t, U u)		// { dg-message "note" }
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
      // { dg-message "candidate" "candidate note" { target *-*-* } 23 }
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

