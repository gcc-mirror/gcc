// { dg-additional-options {-fmodules-ts -fdump-lang-module-alias} }

import "hdr-init-1_a.H";
import "hdr-init-1_b.H";

int bob ()
{
  static int i;

  return ++i;
}

int main ()
{
  return !(var == 1);
}

// { dg-final { scan-lang-dump-times {Reading 1 initializers} 2 module } }

// { dg-final { scan-lang-dump {Read:-[0-9]*'s named merge key \(new\) var_decl:'::var'} module } }
// { dg-final { scan-lang-dump-times {Reading definition var_decl '::var@[^\n]*/hdr-init-1_a.H:1'} 2 module } }

// { dg-final { scan-lang-dump {Read:-[0-9]*'s named merge key \(matched\) var_decl:'::var'} module } }

