// PR 99170
// { dg-module-do link }
// { dg-additional-options -fmodule-header }
// { dg-module-cmi {} }
struct Foo 
{
  Foo () {};
  
};

static Foo __ioinit;
