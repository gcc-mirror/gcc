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

template <int (D::*fun)() const> int Get();

int main () 
{
  Get<&B::I>();   // { dg-error "not a valid template argument" "not valid" }
  // { dg-error "no match" "no match" { target *-*-* } 18 }
  // { dg-message "note" "note" { target *-*-* } 18 }
  Get<&D::I>();   // { dg-error "not a valid template argument" "not valid" }
  // { dg-error "no match" "no match" { target *-*-* } 21 }
  // { dg-message "note" "note" { target *-*-* } 21 }
}
