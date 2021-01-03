// { dg-additional-options {-fmodules-ts -fdump-lang-module-alias} }

import "internal-2_a.H";
import "internal-2_b.H";

int main ()
{
  bob (2);
}

// { dg-final { scan-lang-dump { Read:-1's named merge key \(new\) function_decl:'::bob'} module } }
// { dg-final { scan-lang-dump { Read:-1's named merge key \(matched\) function_decl:'::bob'} module } }
