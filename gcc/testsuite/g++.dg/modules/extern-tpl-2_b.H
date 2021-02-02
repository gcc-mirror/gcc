// { dg-additional-options -fmodule-header }
// { dg-module-cmi {} }
import "extern-tpl-2_a.H";

struct Foo 
{
  TPL<1> m;

  Foo () {m.Source ();};
  
};

static Foo __ioinit;
