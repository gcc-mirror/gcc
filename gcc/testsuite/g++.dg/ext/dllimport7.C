// { dg-do compile { target i?86-*-cygwin* i?86-*-mingw* x86_64-*-mingw* } }

//  Report errors on definition of dllimport'd static data member . 


struct Baz
{
  Baz(int a_ =0) : a(a_) {} 
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

const int Bar::three = 3;       //  { dg-warning "redeclared without dllimport" }
//  { dg-error "definition of static data" "C++ specific error" { target i?86-*-cygwin* i?86-*-mingw* x86_64-*-mingw* } 21 }
				
const Baz Bar::null_baz;	//  { dg-warning "redeclared without dllimport" }

int foo()
{
  Bar foobar;
  const int* baz = &Bar::two; 
  int a = foobar.two;
  int b = foobar.three;
  int c = foobar.null_baz.a;
  return (a + b + c + *baz);
}
