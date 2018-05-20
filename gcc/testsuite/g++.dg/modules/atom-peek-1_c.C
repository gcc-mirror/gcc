// { dg-additional-options "-fmodules-atom -fdump-lang-module" }

import pop;

// { dg-final { scan-lang-dump {Peeking import 'pop'} module } }
// { dg-final { scan-lang-dump {Query BMI 'pop'} module } }
