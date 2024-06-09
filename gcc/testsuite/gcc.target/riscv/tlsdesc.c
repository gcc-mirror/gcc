/* { dg-require-effective-target tls_native } */
/* { dg-options "-O2 -fpic -mtls-dialect=desc --save-temps" } */
/* { dg-require-effective-target fpic } */

#include "tls_1.x"

/* { dg-final { scan-assembler-times "auipc\t\[a-x0-9\]+,%tlsdesc_hi" 1 } } */
/* { dg-final { scan-assembler-times "lw\t\[a-x0-9\]+,%tlsdesc_load_lo" 1 { target { rv32 } } } } */
/* { dg-final { scan-assembler-times "ld\t\[a-x0-9\]+,%tlsdesc_load_lo" 1 { target { rv64 } } } }*/
/* { dg-final { scan-assembler-times "addi\ta0,\[a-x0-9\]+,%tlsdesc_add_lo" 1 } } */
/* { dg-final { scan-assembler-times "jalr\tt0,\[a-x0-9\]+,%tlsdesc_call" 1 } } */
/* { dg-final { cleanup-saved-temps } } */
