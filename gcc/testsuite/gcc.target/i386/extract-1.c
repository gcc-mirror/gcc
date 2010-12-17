/* { dg-do compile } */
/* { dg-options "-O2 -mtune=generic" } */

int
foo (unsigned char x, unsigned char y)
{
   return (x % y) != 0;
}

/* { dg-final { scan-assembler-not "test\[b\]?\[^\\n\]*%\[a-d\]l" } } */
