// { dg-do compile { target i?86-*-cygwin* i?86-*-mingw*} }

//  Report errors on definition of dllimport'd static data member . 


struct Baz
{
  Baz(int a_ =0) : a(a_) {}; 
  int a;
};

class  __declspec(dllimport) Bar
{
  public:
    enum {one = 1};
    static const int two = 2;
    static const int three;
    static const Baz null_baz;
};

const int Bar::three = 3;	//  { dg-error "definition of static data" }
const Baz Bar::null_baz;	//  { dg-error "definition of static data" }


int foo()
{
  Bar foobar;
  const int* baz = &Bar::two; 
  int a = foobar.two;
  int b = foobar.three;
  int c = foobar.null_baz.a;
  return (a + b + c + *baz);
}
