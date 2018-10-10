// { dg-additional-options "-fmodules-atom" }

import "alias-2_a.H";
import "alias-2_b.H";

int a;

#if FROB != 1
#error "FROB != 1"
#endif
#if bink != 6
#error "bink != 6"
#endif

