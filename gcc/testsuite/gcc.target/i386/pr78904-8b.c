/* PR target/78904 */
/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -masm=att" } */
/* { dg-final { scan-assembler-not "movzbl" } } */

struct S1
{
  unsigned char val;
  unsigned char pad1;
  unsigned short pad2;
  unsigned int pad3;
};

struct S2
{
  unsigned char pad1;
  unsigned char val;
  unsigned short pad2;
  unsigned int pad3;
};

struct S1 test (struct S1 a, struct S2 b)
{
  a.val = b.val;

  return a;
}
