// { dg-do compile { target i?86-*-cygwin* i?86-*-mingw* x86_64-*-mingw* } }

// PR 10148  Dllimport attribute of object is overriden by later
// redefinition without attribute.
 
struct Foo
{
  int a;
};

 __attribute__((dllimport)) struct Foo f;

void Bar(void)
{
  void* dummy  = (void*) &f;
}

struct Foo f;	// { dg-warning "referenced with dll linkage" }

// Dllimport'd symbols do not have a constant address, so following
// assignment would require static_initialization_and_destruction
// if attribute is retained. 

void* dummy = &f;
