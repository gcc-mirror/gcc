/* Verify that we do not generate a malformed ldxdw instruction
   with a constant instead of register + offset.  */

/* { dg-do compile } */
/* { dg-options "-O2" } */

/* { dg-final { scan-assembler-times "ldxdw\t%r.,\\\[%r.+0\\\]" 1 } } */
/* { dg-final { scan-assembler-not "ldxdw\t%r.,\[0-9\]+" } } */

unsigned long long test () {
  return *((unsigned long long *) 0x4000);
}
