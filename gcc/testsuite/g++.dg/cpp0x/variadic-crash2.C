// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin: PR c++/39637
// { dg-do compile }
// { dg-options "-std=gnu++11" }

template<class... Types>
void
f(Types...)
{
  enum {e = sizeof(Types)}; // { dg-error "parameter packs not expanded with '...'" }
  enum {e1 = sizeof...(Types)};
}

int
main()
{
    f(0);
}

// { dg-message "note" "Types" { target *-*-* } 10 }
