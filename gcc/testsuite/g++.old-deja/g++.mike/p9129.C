// { dg-do assemble  }
// { dg-options "-ansi -pedantic-errors" }
// prms-id: 9129

class Foo {
public:
  int DoSomething();
};

int (Foo::*pA)() = { &Foo::DoSomething };
int (Foo::*X[1])(int) = { { &Foo::DoSomething } };		    // { dg-error "" } 
int (Foo::*Y[])(int) = { { &Foo::DoSomething, &Foo::DoSomething, 0 } }; // { dg-error "" } 
