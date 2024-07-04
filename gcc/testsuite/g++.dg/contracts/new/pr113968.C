// check that an invalid contract condition doesn't cause an ICE
// { dg-do compile }
// { dg-options "-std=c++23 -fcontracts -fcontracts-nonattr" }
struct A {
  A(A &);
};
struct S{
  void f(A a) pre(a) // { dg-error "could not convert" }
              pre(a.b) // { dg-error "has no member" }
  {

  }
};
void f(A a) pre(a) // { dg-error "could not convert" }
	    pre(a.b) // { dg-error "has no member" }
	    {
  contract_assert(a); // { dg-error "could not convert" }
  contract_assert(a.b); // { dg-error "has no member" }
}

int main()
{
}
