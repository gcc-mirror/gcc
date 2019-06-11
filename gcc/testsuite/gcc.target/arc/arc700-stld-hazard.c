/* { dg-do compile } */
/* { dg-skip-if "" { ! { clmcpu } } } */
/* { dg-options "-mcpu=arc700 -mno-sdata -O2" } */

volatile int a;
volatile int b;

void
foo ()
{
  a = 1;
  b = a;
}

/* { dg-final { scan-assembler "st\\s+r\[0-9\]+,\\\[@a\\\]\\.*\[^\n\]*\n\[ \t\]+nop_s\[^\n\]*\n\[ \t\]+nop_s\[^\n\]*\n\[ \t\]+ld\\s+r\[0-9\]+,\\\[@a\\\]" } } */
