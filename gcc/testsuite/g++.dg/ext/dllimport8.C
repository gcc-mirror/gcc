//  PR c++/8378
//  Ignore  dllimport of static members if marked inlined.
//  or if definition follows  declaration in dllimported class.

// { dg-do compile { target i?86-*-cygwin* i?86-*-mingw*} }
// { dg-options { -Wall -W } }

struct  __attribute__((dllimport)) Foo
 {
    static int static_int;
    static void static_func1();
    static void static_func2();
 };

void Foo::static_func1()
  {		//  { dg-warning "defined" }
  }

inline void Foo::static_func2()
 {		//  { dg-warning "inline function" }
 }

void testfoo()
{ 
  Foo::static_func1();
  Foo::static_func2();
}

// { dg-final { scan-assembler-not "__imp__" } }
