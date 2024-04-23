/* { dg-do compile } */
/* { dg-additional-options "-fno-tree-forwprop" } */

unsigned long x;
unsigned char y;

void
foo (void)
{
  unsigned long tt = y;
  tt+=255;
  unsigned short t1 = tt;
  t1 = 254 - t1;
  tt += ((unsigned long)t1);
  x = tt;
}
