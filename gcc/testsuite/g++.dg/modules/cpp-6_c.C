// { dg-do preprocess }
// { dg-additional-options "-fmodules-ts -isystem [srcdir]" }

#define empty
#define nop(X) X

ONE bibity bobity
import <cpp-6_a.H>;
TWO bibity bobity
import empty nop(<bibity>);
THREE bibity bobity
import empty <bobity.H>;
FOUR bibity bobity

// { dg-final { scan-file cpp-6_c.i {ONE bibity bobity\n} } }
// { dg-final { scan-file cpp-6_c.i {TWO cpp-6_b.H bobity\n} } }
// { dg-final { scan-file cpp-6_c.i {THREE cpp-6_b.H cpp-6_b\n} } }
// { dg-final { scan-file cpp-6_c.i {FOUR cpp-6_b.H cpp-6_b\n} } }
