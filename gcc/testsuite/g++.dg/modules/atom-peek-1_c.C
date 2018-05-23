// { dg-additional-options "-fmodules-atom -fdump-lang-module" }

import pop;

// { dg-final { scan-lang-dump {Server request:PEEK BMI pop } module } }
// { dg-final { scan-lang-dump {Server request:BMI pop } module } }
