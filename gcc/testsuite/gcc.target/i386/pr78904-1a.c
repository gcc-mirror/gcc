/* PR target/78904 */
/* { dg-do compile } */
/* { dg-options "-O2 -masm=att" } */
/* { dg-final { scan-assembler-not "movzbl" } } */
/* { dg-final { scan-assembler-not "movb" } } */

struct S1
{
  unsigned char pad1;
  unsigned char val;
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
