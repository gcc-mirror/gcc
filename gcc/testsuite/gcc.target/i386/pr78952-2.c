/* PR target/78952 */
/* { dg-do compile } */
/* { dg-options "-O2 -masm=att" } */
/* { dg-additional-options "-mregparm=3" { target ia32 } } */
/* { dg-final { scan-assembler-not "sarl" } } */

struct S1
{
  char pad1;
  char val;
  short pad2;
};

struct S1 foo (struct S1 a, int b)
{
  a.val = b >> 8;

  return a;
}

/* { dg-final { scan-assembler "\[ \t\]movb\[ \t\]+%.h, %.h" } } */
