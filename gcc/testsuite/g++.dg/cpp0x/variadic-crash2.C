// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin: PR c++/39637
// { dg-options "-std=gnu++0x" }
// { dg-do "compile" }

template<class... Types>
void
f(Types...)
{
  enum {e = sizeof(Types)}; // { dg-error "parameter packs not expanded with|note" }
  enum {e1 = sizeof...(Types)};
}

int
main()
{
    f(0);
}
