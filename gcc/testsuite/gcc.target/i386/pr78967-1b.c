/* PR target/78967 */
/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -masm=att" } */
/* { dg-final { scan-assembler-not "movzbl" } } */

struct S1
{
  unsigned char pad1;
  unsigned char val;
  unsigned short pad2;
  unsigned int pad3;
};

struct S1 foo (struct S1 a, struct S1 b)
{
  a.val = b.val;

  return a;
}

/* { dg-final { scan-assembler "\[ \t\]movb\[ \t\]+%.h, %.h" } } */
