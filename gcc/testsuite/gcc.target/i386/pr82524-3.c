/* PR target/82524 */
/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -masm=att" } */
/* { dg-final { scan-assembler-not "movzbl" } } */

struct S
{
  unsigned char pad1;
  unsigned char val;
  unsigned short pad2;
  unsigned int pad3;
};

struct S
test_and (struct S a, struct S b, struct S c)
{
  a.val = b.val & c.val;

  return a;
}

/* { dg-final { scan-assembler "\[ \t\]andb" } } */

struct S
test_or (struct S a, struct S b, struct S c)
{
  a.val = b.val | c.val;

  return a;
}

/* { dg-final { scan-assembler "\[ \t\]orb" } } */

struct S
test_xor (struct S a, struct S b, struct S c)
{
  a.val = b.val ^ c.val;

  return a;
}

/* { dg-final { scan-assembler "\[ \t\]xorb" } } */
