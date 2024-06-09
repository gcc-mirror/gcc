/* PR target/78952 */
/* { dg-do compile } */
/* { dg-options "-O2 -masm=att" } */
/* { dg-final { scan-assembler-not "mov\[sz\]bl" } } */
/* { dg-final { scan-assembler-not "movb" } } */

struct S1
{
  signed char pad1;
  signed char val;
  signed short pad2;
};

struct S1 test_and (struct S1 a, struct S1 b)
{
  a.val &= b.val;

  return a;
}

/* { dg-final { scan-assembler "\[ \t\]andb" } } */

struct S1 test_or (struct S1 a, struct S1 b)
{
  a.val |= b.val;

  return a;
}

/* { dg-final { scan-assembler "\[ \t\]orb" } } */

struct S1 test_xor (struct S1 a, struct S1 b)
{
  a.val ^= b.val;

  return a;
}

/* { dg-final { scan-assembler "\[ \t\]xorb" } } */

struct S1 test_add (struct S1 a, struct S1 b)
{
  a.val += b.val;

  return a;
}

/* { dg-final { scan-assembler "\[ \t\]addb" } } */

struct S1 test_sub (struct S1 a, struct S1 b)
{
  a.val -= b.val;

  return a;
}

/* { dg-final { scan-assembler "\[ \t\]subb" } } */
