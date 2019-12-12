// { dg-do compile }
// Origin: <marco dot franzen at bigfoot dot com> 
// PR c++/10126: Handle ptmf default conversions while matching a template 
//  argument

struct B 
{
  int I () const;
  int I ();
};

struct D : B {};

template <int (D::*fun)() const> int Get(); // { dg-message "note" }

int main () 
{
  Get<&B::I>();   // { dg-error "template argument|convert" "not valid" }
  // { dg-error "no match" "no match" { target *-*-* } .-1 }
  Get<&D::I>();   // { dg-error "template argument|convert" "not valid" }
  // { dg-error "no match" "no match" { target *-*-* } .-1 }
}
