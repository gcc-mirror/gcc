/* { dg-do compile } */
/* { dg-options "-mlittle-endian" } */

float f;
float a() { f = 1.0; return 1.0; }
float b() { f = 2.0; return 2.0; }
float c() { f = 2.0; return 3.0; }
float d() { f = 3.0; return 3.0; }

/* { dg-final { scan-assembler-times "lddw\t%r.,0x3f800000" 2 } } */
/* { dg-final { scan-assembler-times "lddw\t%r.,0x40000000" 3 } } */
/* { dg-final { scan-assembler-times "lddw\t%r.,0x40400000" 3 } } */
