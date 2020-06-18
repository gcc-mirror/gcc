/* PR target/78967 */
/* { dg-do compile } */
/* { dg-options "-O2 -masm=att" } */
/* { dg-require-effective-target nonpic } */
/* { dg-final { scan-assembler-not "movzbl" } } */

typedef __SIZE_TYPE__ size_t;

struct S1
{
  unsigned char pad1;
  unsigned char val;
};

extern unsigned char t[256];

struct S1 foo (struct S1 a, size_t i)
{
  a.val = t[i];

  return a;
}

/* { dg-final { scan-assembler "\[ \t\]movb\[ \t\]+t\[^\n\r]*, %.h" } } */
