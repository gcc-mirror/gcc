// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin: PR c++/39639
// { dg-options "-std=c++0x" }
// { dg-do "compile" }

template <class... Types>
struct S
  : S<...Types>, // { dg-error "expected parameter pack before '...'" }
    S<...Types...>, // { dg-error "expected parameter pack before '...'" }
    S<...> // { dg-error "expected parameter pack before '...'" }
{
  static int f () { return 1;}
};

int
main ()
{
  return S<void>::f ();
}

