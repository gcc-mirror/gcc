// { dg-additional-options "-fmodules-ts" }
import "alias-2_b.H";
import "alias-2_a.H";

int a;

#if FROB != 2
#error "FROB != 2"
#endif
#if bink != 7
#error "bink != 7"
#endif

