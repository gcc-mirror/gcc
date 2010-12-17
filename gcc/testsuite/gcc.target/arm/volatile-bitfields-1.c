/* { dg-require-effective-target arm_eabi } */
/* { dg-do compile } */
/* { dg-options "-O2" } */

typedef struct {
  char a:1;
  char b:7;
  int c;
} BitStruct;

volatile BitStruct bits;

int foo ()
{
  return bits.b;
}

/* { dg-final { scan-assembler "ldrb\[\\t \]+\[^\n\]*,\[\\t \]*\\\[\[^\n\]*\\\]" } } */
