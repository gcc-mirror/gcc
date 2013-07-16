/* { dg-do compile { target *-*-darwin* } } */
/* { dg-skip-if "" { *-*-* } { "-fgnu-runtime" } { "" } } */

#include <Foundation/NSObject.h>
main() { [NSObject new]; }

/* { dg-final { scan-assembler-not "L_objc_msgSend\\\$non_lazy_ptr" } } */
