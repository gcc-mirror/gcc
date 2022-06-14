/* { dg-do compile } */
/* { dg-options "-O2 -save-temps" } */

#define ALIGN 16
#define EXTRA

#include "bitfield-abi-warning.h"

/* In f1, f2, f4, f8, f16, f16p (and stdarg versions):  */
/* { dg-final { scan-assembler-times "and\tw0, w2, 1" 12 { xfail *-*-* } } } */
/* In fp, f1p, f2p, f4p, f8p (and stdarg versions):  */
/* { dg-final { scan-assembler-times "and\tw0, w1, 1" 10 { xfail *-*-* } } } */

/* Bitfield parameter in registers.  */
/* { dg-message {parameter passing for argument of type 'struct S1' changed in GCC 9.1} "" { target *-*-* } 47 } f1 */
/* { dg-message {parameter passing for argument of type 'struct S2' changed in GCC 9.1} "" { target *-*-* } 48 } f2 */
/* { dg-message {parameter passing for argument of type 'struct S4' changed in GCC 9.1} "" { target *-*-* } 49 } f4 */
/* { dg-message {parameter passing for argument of type 'struct S8' changed in GCC 9.1} "" { target *-*-* } 50 } f8 */

/* No change in parameter passing in GCC 9.1 for lines 53-57 (fp, f1p, f2p,
   f4p, f8p) (because the argument fits in a single register).  Should not
   warn, but aarch64_function_arg_boundary would need to take the argument size
   into account as well as whether it's passed via registers or the stack.  */
/* { dg-message {parameter passing for argument of type 'struct Sp' changed in GCC 9.1} "" { target *-*-* } 53 }  fp */
/* { dg-message {parameter passing for argument of type 'struct S1p' changed in GCC 9.1} "" { target *-*-* } 54 } f1p */
/* { dg-message {parameter passing for argument of type 'struct S2p' changed in GCC 9.1} "" { target *-*-* } 55 } f2p */
/* { dg-message {parameter passing for argument of type 'struct S4p' changed in GCC 9.1} "" { target *-*-* } 56 } f4p */
/* { dg-message {parameter passing for argument of type 'struct S8p' changed in GCC 9.1} "" { target *-*-* } 57 } f8p */

/* Bitfield call argument in registers.  */
/* { dg-message {parameter passing for argument of type 'struct S1' changed in GCC 9.1} ""  { target *-*-* } 60 } g1 */
/* { dg-message {parameter passing for argument of type 'struct S2' changed in GCC 9.1} ""  { target *-*-* } 61 } g2 */
/* { dg-message {parameter passing for argument of type 'struct S4' changed in GCC 9.1} ""  { target *-*-* } 62 } g4 */
/* { dg-message {parameter passing for argument of type 'struct S8' changed in GCC 9.1} ""  { target *-*-* } 63 } g8 */

/* No change in parameter passing in GCC 9.1 for lines 66-70 (gp, g1p, g2p,
   g4p, g8p), no warning expected.  */


/* Bitfield parameter in stack.  */
/* { dg-message {parameter passing for argument of type 'struct S1' changed in GCC 9.1} "" { target *-*-* } 74 } f1_stack */
/* { dg-message {parameter passing for argument of type 'struct S2' changed in GCC 9.1} "" { target *-*-* } 75 } f2_stack */
/* { dg-message {parameter passing for argument of type 'struct S4' changed in GCC 9.1} "" { target *-*-* } 76 } f4_stack */
/* { dg-message {parameter passing for argument of type 'struct S8' changed in GCC 9.1} "" { target *-*-* } 77 } f8_stack */

/* { dg-message {parameter passing for argument of type 'struct Sp' changed in GCC 9.1} "" { target *-*-* } 80 }  fp_stack */
/* { dg-message {parameter passing for argument of type 'struct S1p' changed in GCC 9.1} "" { target *-*-* } 81 } f1p_stack */
/* { dg-message {parameter passing for argument of type 'struct S2p' changed in GCC 9.1} "" { target *-*-* } 82 } f2p_stack */
/* { dg-message {parameter passing for argument of type 'struct S4p' changed in GCC 9.1} "" { target *-*-* } 83 } f4p_stack */
/* { dg-message {parameter passing for argument of type 'struct S8p' changed in GCC 9.1} "" { target *-*-* } 84 } f8p_stack */

/* Bitfield call argument in stack.  */
/* { dg-message {parameter passing for argument of type 'struct S1' changed in GCC 9.1} ""  { target *-*-* } 87 } g1_stack */
/* { dg-message {parameter passing for argument of type 'struct S2' changed in GCC 9.1} ""  { target *-*-* } 88 } g2_stack */
/* { dg-message {parameter passing for argument of type 'struct S4' changed in GCC 9.1} ""  { target *-*-* } 89 } g4_stack */
/* { dg-message {parameter passing for argument of type 'struct S8' changed in GCC 9.1} ""  { target *-*-* } 90 } g8_stack */

/* { dg-message {parameter passing for argument of type 'struct Sp' changed in GCC 9.1} "" { target *-*-* } 93 }  gp_stack */
/* { dg-message {parameter passing for argument of type 'struct S1p' changed in GCC 9.1} "" { target *-*-* } 94 } g1p_stack */
/* { dg-message {parameter passing for argument of type 'struct S2p' changed in GCC 9.1} "" { target *-*-* } 95 } g2p_stack */
/* { dg-message {parameter passing for argument of type 'struct S4p' changed in GCC 9.1} "" { target *-*-* } 96 } g4p_stack */
/* { dg-message {parameter passing for argument of type 'struct S8p' changed in GCC 9.1} "" { target *-*-* } 97 } g8p_stack */


/* Bitfield parameter in stdarg.  */
/* { dg-message {parameter passing for argument of type 'struct S1' changed in GCC 9.1} "" { target *-*-* } 101 } f1_stdarg */
/* { dg-message {parameter passing for argument of type 'struct S2' changed in GCC 9.1} "" { target *-*-* } 102 } f2_stdarg */
/* { dg-message {parameter passing for argument of type 'struct S4' changed in GCC 9.1} "" { target *-*-* } 103 } f4_stdarg */
/* { dg-message {parameter passing for argument of type 'struct S8' changed in GCC 9.1} "" { target *-*-* } 104 } f8_stdarg */

/* Parameter passing for these should not have changed in GCC 9.1 (PR 105549).
   Fortunately we warn. Note the discrepancy with lines 120-124 below: we warn
   in the callee, but not in the caller.  */
/* { dg-message {parameter passing for argument of type 'struct Sp' changed in GCC 9.1} "" { target *-*-* } 107 }  fp_stdarg */
/* { dg-message {parameter passing for argument of type 'struct S1p' changed in GCC 9.1} "" { target *-*-* } 108 } f1p_stdarg */
/* { dg-message {parameter passing for argument of type 'struct S2p' changed in GCC 9.1} "" { target *-*-* } 109 } f2p_stdarg */
/* { dg-message {parameter passing for argument of type 'struct S4p' changed in GCC 9.1} "" { target *-*-* } 110 } f4p_stdarg */
/* { dg-message {parameter passing for argument of type 'struct S8p' changed in GCC 9.1} "" { target *-*-* } 111 } f8p_stdarg */

/* Bitfield call argument in stdarg.  */
/* { dg-message {parameter passing for argument of type 'struct S1' changed in GCC 9.1} ""  { target *-*-* } 114 } g1_stdarg */
/* { dg-message {parameter passing for argument of type 'struct S2' changed in GCC 9.1} ""  { target *-*-* } 115 } g2_stdarg */
/* { dg-message {parameter passing for argument of type 'struct S4' changed in GCC 9.1} ""  { target *-*-* } 116 } g4_stdarg */
/* { dg-message {parameter passing for argument of type 'struct S8' changed in GCC 9.1} ""  { target *-*-* } 117 } g8_stdarg */

/* No change in parameter passing in GCC 9.1 for lines 120-124 (gp_stdarg
   g1p_stdarg, g2p_stdarg, g4p_stdarg, g8p_stdarg), no warning expected.  */
