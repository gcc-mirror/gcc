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

// { dg-final { scan-lang-dump {Voldemort decl:0 \[3\] '::TPL@[^\n]*/late-ret-2_a.H:2<int>' \(merged\)} module } }
// { dg-final { scan-lang-dump {Read:-3's named merge key \(matched\) template_decl:'::Foo@[^\n]*/late-ret-2_b.H:3'\n  Deduping '::Foo@[^\n]*/late-ret-2_a.H:2'\n} module } }
