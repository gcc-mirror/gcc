// { dg-additional-options "-fmodules -fdump-lang-module" }

import M2;

A::AT var1;
AT var2;			// { dg-error "AT" }

// { dg-final { scan-lang-dump {Ignoring using-directives because module M1 is not visible} module } }
