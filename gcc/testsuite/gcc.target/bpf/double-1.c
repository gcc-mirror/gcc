/* { dg-do compile } */
/* { dg-options "-mlittle-endian -masm=normal" } */

double f;
double a() { f = 1.0; return 1.0; }
double b() { f = 2.0; return 2.0; }
double c() { f = 2.0; return 3.0; }
double d() { f = 3.0; return 3.0; }

/* { dg-final { scan-assembler-times "lddw\t%r.,0x3ff0000000000000" 2 } } */
/* { dg-final { scan-assembler-times "lddw\t%r.,0x4000000000000000" 3 } } */
/* { dg-final { scan-assembler-times "lddw\t%r.,0x4008000000000000" 3 } } */
