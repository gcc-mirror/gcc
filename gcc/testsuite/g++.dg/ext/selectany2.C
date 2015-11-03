// { dg-do compile { target i?86-pc-cygwin } }
// { dg-do compile { target i?86-*-mingw* x86_64-*-mingw* } }

// Check for errors with invalid usage of selectany attribute.

extern int foo;
__declspec (selectany) int foo = 1;  // OK

struct d
{
  static int foo;
};
__declspec (selectany) int d::foo = 1;  // OK 

struct  f
{
  int i;
};
__declspec (selectany) struct f  F= {1}; // OK

__declspec (selectany) int boo;	//  OK

__declspec (selectany) static int bar = 1; // { dg-error "selectany" }
int use_bar = bar;  //  Avoid defined but not used warning. 

int baz()
{
  __declspec (selectany)  int foo = 1;  // { dg-error "selectany" }
  return foo;
}
