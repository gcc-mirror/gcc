// { dg-do run  }
// PRMS Id: 4341
// Bug: Instantiating a template in the middle of processing the functions
// from another template screws up lineno/input_filename.

#pragma implementation "C.h"
#line 1 "A.h"
#pragma interface
template <class T> class A {};
#line 1 "C.h"
#pragma interface
template <class T> class C
{
public:
  C() { A<T> *ap; }
  ~C() { }
};
#line 18 "inline.C"
int main()
{
  C<int> c;
}
