// { dg-do compile { target c++20 } }

template<auto>
struct S { };

struct R { };

void
g (void)
{
  S<R()> s; // { dg-error "mismatch" }
// { dg-message "treated as function" "note" { target *-*-* } .-1 }
  S<R{}> s2;
  S<int()> s3; // { dg-error "mismatch" }
// { dg-message "treated as function" "note" { target *-*-* } .-1 }
  S<int{}> s4;
}
