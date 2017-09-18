/* { dg-do compile } */
/* { dg-options "-mcpu=arc700 -mno-sdata" } */

volatile int a;
volatile int b;

void
foo ()
{
  a = 1;
  b = a;
}

/* { dg-final { scan-assembler "st r\[0-9\]+,\\\[@a\\\]\[^\n\]*\n\[ \t\]+nop_s\[^\n\]*\n\[ \t\]+nop_s\[^\n\]*\n\[ \t\]+ld r\[0-9\]+,\\\[@a\\\]" } } */
