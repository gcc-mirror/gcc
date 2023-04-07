/* { dg-do compile } */
/* { dg-options "-O2 -fpeephole2 -mabi=windowed" } */

/* cannot be processed: due to violate 'a' constraint of the destination operand of the stack adjustment instruction.  */
void test(void) {
  int buffer[8192];
  asm volatile ("" : : "m"(buffer));
}

/* { dg-final { scan-assembler-times "movsp" 1 } } */
