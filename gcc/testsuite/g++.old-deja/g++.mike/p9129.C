// Build don't link:
// Special g++ Options: -ansi -pedantic-errors
// prms-id: 9129

class Foo {
public:
  int DoSomething();
};

int (Foo::*pA)() = { &Foo::DoSomething };	
int (Foo::*X[1])(int) = { { &Foo::DoSomething } };		    // ERROR - 
int (Foo::*Y[])(int) = { { &Foo::DoSomething, &Foo::DoSomething, 0 } }; // ERROR - 
