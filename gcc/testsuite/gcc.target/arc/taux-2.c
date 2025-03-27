/* { dg-do compile } */
/* { dg-options "-O1" } */

#define __aux(r) __attribute__((aux(r)))
static volatile __aux(0x1000) int var;

int foo (void)
{
  var++;
}

/* { dg-final { scan-assembler-times "sr" 1 } } */
/* { dg-final { scan-assembler-times "lr" 1 } } */
/* { dg-final { scan-assembler "4096" } } */
/* { dg-final { scan-assembler-not "\\.type\tvar, @object" } } */
