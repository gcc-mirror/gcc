// { dg-additional-options "-fmodules-ts -fdump-lang-module-alias" }

import "late-ret-3_a.H";
import "late-ret-3_b.H";

struct Arg 
{
  int type;
};

int main ()
{
  Arg arg;

  int j = Foo (arg);

  Bar (arg); // { dg-error "ambiguous" }

  return 0;
}

// { dg-final { scan-lang-dump {Read:-[0-9]*'s named merge key \(matched\) template_decl:'::template Foo'\n  Deduping '::template Foo@[^\n]*/late-ret-3_a.H:.'\n} module } }
