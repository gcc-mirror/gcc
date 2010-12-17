/* { dg-do compile } */
/* { dg-options "-O2 -mtune=generic" } */

int
foo (unsigned char x, unsigned char y)
{
   return (x % y) > 4;
}

/* { dg-final { scan-assembler-times "cmp\[b\]?\[^\\n\]*%\[a-d\]h" 1 } } */
/* { dg-final { scan-assembler-not "cmp\[b\]?\[^\\n\]*%\[a-d\]l" } } */
