// PR c++/57638

template<int x>
struct S {};

template<long long i>
void g(S<i>);

void f()
{
  S<1000> t;
  g(t);         // { dg-error "no matching" }
}  // { dg-message "mismatched types 'long long int' and 'int'" "" { target *-*-* } .-1 }
