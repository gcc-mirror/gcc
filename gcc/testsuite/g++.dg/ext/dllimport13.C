//  PR c++/34749
//  Ensure dllimport is handled correctly for friends

// { dg-do compile { target i?86-*-cygwin* i?86-*-mingw*} }

int  __declspec (dllimport) bar();
int  __declspec (dllimport) baz();

class Foo
{
//  MS requires that the dllimport attribute be specified on each declaration 
    friend  int __declspec (dllimport) bar();
    friend int  baz();  //  { dg-warning "dllimport ignored" }
};
