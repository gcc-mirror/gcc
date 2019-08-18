/* PR target/91188 */
/* { dg-do compile } */
/* { dg-options "-O2 -masm=att" } */
/* { dg-additional-options "-mregparm=3" { target ia32 } } */
/* { dg-final { scan-assembler-not "movzwl" } } */
/* { dg-final { scan-assembler-not "movw" } } */

struct S
{
  unsigned short val;
  unsigned short pad;
};

struct S
test_and (struct S a, unsigned short b)
{
  a.val &= b;

  return a;
}

/* { dg-final { scan-assembler "\[ \t\]andw" } } */

struct S
test_or (struct S a, unsigned short b)
{
  a.val |= b;

  return a;
}

/* { dg-final { scan-assembler "\[ \t\]orw" } } */

struct S
test_xor (struct S a, unsigned short b)
{
  a.val ^= b;

  return a;
}

/* { dg-final { scan-assembler "\[ \t\]xorw" } } */

struct S
test_add (struct S a, unsigned short b)
{
  a.val += b;

  return a;
}

/* { dg-final { scan-assembler "\[ \t\]addw" } } */

struct S
test_sub (struct S a, unsigned short b)
{
  a.val -= b;

  return a;
}

/* { dg-final { scan-assembler "\[ \t\]subw" } } */
