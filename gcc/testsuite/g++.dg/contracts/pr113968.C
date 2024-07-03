// check that an invalid contract condition doesn't cause an ICE
// { dg-options "-std=c++2a -fcontracts " }
// { dg-do compile }

struct A {
  A(A &);
};
struct S{
  void f(A a) [[ pre : a]] // { dg-error "could not convert" }
              [[ pre : a.b]] // { dg-error "has no member" }
  {

  }
};
void f(A a) [[ pre : a]] // { dg-error "could not convert" }
	     [[ pre : a.b]] // { dg-error "has no member" }
	    {
  [[ assert : a ]] ; // { dg-error "could not convert" }
  [[ assert : a.b ]]; // { dg-error "has no member" }
}

int main()
{
}




