/* { dg-do compile } */
/* { dg-options "-mpure-code" } */

int x;
int f1 (void) { return x; }

/* We expect only one indirect load like ldr r3, [r3]. In some
   configurations there is an additional ldr rX, [sp], #4 which is not
   related to what we check here, so make sure not to match it.  */
/* { dg-final { scan-assembler-times "ldr\tr\[0-9\]+, \\\[r\[0-9\]+\\\]" 1 } } */
