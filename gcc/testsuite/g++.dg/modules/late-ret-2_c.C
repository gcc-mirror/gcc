// { dg-additional-options "-fmodules-ts -fdump-lang-module-alias" }

import "late-ret-2_a.H";
import "late-ret-2_b.H";

int main ()
{
  int *p = 0;
  int j = Foo (p);

  Bar (p); // { dg-error "ambiguous" }

  return 0;
}

// { dg-final { scan-lang-dump {Read:-[0-9]*'s named merge key \(matched\) template_decl:'::template Foo'\n  Deduping '::template Foo@[^\n]*/late-ret-2_a.H:.'\n} module } }
