/* { dg-do compile } */
/* { dg-options "-O3 -march=z9-ec -fPIC" } */

#include "call.h"

/* { dg-final { scan-assembler {brasl\t%r\d+,foo@PLT\n} } } */
/* { dg-final { scan-assembler {larl\t%r\d+,foo@GOTENT\n} } } */

/* { dg-final { scan-assembler {brasl\t%r\d+,foostatic@PLT\n} { target lp64 } } } */
/* { dg-final { scan-assembler {brasl\t%r\d+,foostatic\n} { target { ! lp64 } } } } */
/* { dg-final { scan-assembler {larl\t%r2,foostatic@PLT\n} { target lp64 } } } */
/* { dg-final { scan-assembler {larl\t%r2,foostatic\n} { target { ! lp64 } } } } */

/* { dg-final { scan-assembler {brasl\t%r\d+,fooweak@PLT\n} } } */
/* { dg-final { scan-assembler {larl\t%r\d+,fooweak@GOTENT\n} } } */

/* { dg-final { scan-assembler {foos:\n\t.quad\tfoo\n\t.quad\tfoostatic\n\t.quad\tfooweak\n} { target lp64 } } } */
/* { dg-final { scan-assembler {foos:\n\t.long\tfoo\n\t.long\tfoostatic\n\t.long\tfooweak\n} { target { ! lp64 } } } } */
