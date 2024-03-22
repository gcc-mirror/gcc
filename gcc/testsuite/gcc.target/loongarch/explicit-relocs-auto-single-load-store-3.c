/* { dg-do compile } */
/* { dg-options "-O2 -mexplicit-relocs=auto -fdump-rtl-final" } */
/* { dg-final { scan-rtl-dump-times "mem/v/c" 2 "final" } } */
/* { dg-final { scan-assembler-not "la\\.local" } } */

volatile unsigned long counter;

unsigned long
read (void)
{
  return counter;
}

void
clear (void)
{
  counter = 0;
}
