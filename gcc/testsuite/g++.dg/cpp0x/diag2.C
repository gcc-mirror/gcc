// { dg-do compile { target c++11 } }

struct A {};

// We shouldn't arbitarily choose which of these is better.
void f (A&);
void f (const A&&);

// But do prefer the lvalue overload here.
void g (A&);
void g (A&&);
int main()
{
  const A a;
  f(a);				// { dg-error "no match" }
  // { dg-error "qualifiers" "" { target *-*-* } 15 }
  // { dg-error "lvalue" "" { target *-*-* } 15 }
  g(a);				// { dg-error "qualifiers" }
}
