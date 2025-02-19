/* { dg-do compile } */
/* { dg-options "-O3 -march=z10 -mzarch -fPIC -mno-pic-data-is-text-relative" } */

#include "call.h"

/* { dg-final { scan-assembler {brasl\t%r\d+,foo@PLT\n} } } */
/* { dg-final { scan-assembler {lgrl\t%r2,foo@GOTENT\n} { target lp64 } } } */
/* { dg-final { scan-assembler {lrl\t%r2,foo@GOTENT\n} { target { ! lp64 } } } } */

/* { dg-final { scan-assembler {brasl\t%r\d+,foostatic@PLT\n} } } */
/* { dg-final { scan-assembler {larl\t%r2,foostatic\n} } } */

/* { dg-final { scan-assembler {brasl\t%r\d+,fooweak@PLT\n} } } */
/* { dg-final { scan-assembler {lgrl\t%r2,fooweak@GOTENT\n} { target lp64 } } } */
/* { dg-final { scan-assembler {lrl\t%r2,fooweak@GOTENT\n} { target { ! lp64 } } } } */

/* { dg-final { scan-assembler {foos:\n\t.quad\tfoo\n\t.quad\tfoostatic\n\t.quad\tfooweak\n} { target lp64 } } } */
/* { dg-final { scan-assembler {foos:\n\t.long\tfoo\n\t.long\tfoostatic\n\t.long\tfooweak\n} { target { ! lp64 } } } } */
