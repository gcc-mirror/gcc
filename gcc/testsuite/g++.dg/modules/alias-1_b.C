// { dg-additional-options "-fdump-lang-module -fmodule-mapper=|cxx-mapper\\ -f\\ $srcdir/g++.dg/modules/alias-1.map" }

import "alias-1_a.H";
import <alias-1_a.H>;

int main ()
{
  frob ();
}

// { dg-final { scan-lang-dump {Controlling macro is ALIAS_1_A} module } }
// { dg-final { scan-lang-dump {<alias-1_a.H> is an alias of "alias-1_a.H"} module } }
