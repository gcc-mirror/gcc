// { dg-additional-options "-fmodules-ts" }

export module edith;
// { dg-module-bmi edith }

#define STUART(X) X

import STUART(agnes);

export void STUART(gru) ();
