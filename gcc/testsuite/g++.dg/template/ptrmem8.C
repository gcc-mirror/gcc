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
  Get<&B::I>();   // { dg-error "" }
  Get<&D::I>();   // { dg-error "" }
}
