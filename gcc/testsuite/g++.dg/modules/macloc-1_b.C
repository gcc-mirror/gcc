// { dg-additional-options -fmodules-atom }
// { dg-module-bmi edith }

export module edith;

#define STUART(X) X

import STUART(agnes);

export void STUART(gru) ();
