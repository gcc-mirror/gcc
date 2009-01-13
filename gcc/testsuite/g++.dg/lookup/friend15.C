// Origin: PR c++/35109
// { dg-do compile }

void foo()
{
  struct A
  {
    friend class B;
  };
  B::B() {} // { dg-error "has not been declared" }
// { dg-error "expected" "expected" { target *-*-* } 10 }
}
