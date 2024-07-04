/* { dg-do compile { target { ! avr_tiny } } } */
/* { dg-options "-std=gnu99 -fdata-sections" } */

__attribute__((__section__("fffsection")))
const __flash char fff = 123;

const __flash char ppp = 124;

/* { dg-final { scan-assembler ".section	fffsection,\"a\",@progbits" } } */
/* { dg-final { scan-assembler ".section	.progmem.data.ppp,\"a\",@progbits" } } */
