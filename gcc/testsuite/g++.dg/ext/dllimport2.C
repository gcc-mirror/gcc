// { dg-do compile { target i?86-*-cygwin* i?86-*-mingw*} }

// PR 9738  Dllimport attribute is overriden by later definition

void __attribute__((dllimport)) Bar(void);
 
 void Foo(void)
 {
     Bar();
 }
 
 void Bar(void)
 {
 }
 
