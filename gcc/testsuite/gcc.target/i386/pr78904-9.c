/* PR target/78904 */
/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -masm=att" } */
/* { dg-final { scan-assembler-not "movzbl" } } */

struct S1
{
  unsigned char val;
  unsigned char pad1;
  unsigned short pad2;
};

struct S2
{
  unsigned char pad1;
  unsigned char val;
  unsigned short pad2;
};

struct S1 test_and (struct S1 a, struct S2 b, struct S2 c)
{
  a.val = b.val & c.val;

  return a;
}

/* { dg-final { scan-assembler "\[ \t\]andb" } } */

struct S1 test_or (struct S1 a, struct S2 b, struct S2 c)
{
  a.val = b.val | c.val;

  return a;
}

/* { dg-final { scan-assembler "\[ \t\]orb" } } */

struct S1 test_xor (struct S1 a, struct S2 b, struct S2 c)
{
  a.val = b.val ^ c.val;

  return a;
}

/* { dg-final { scan-assembler "\[ \t\]xorb" } } */

struct S1 test_add (struct S1 a, struct S2 b, struct S2 c)
{
  a.val = b.val + c.val;

  return a;
}

/* { dg-final { scan-assembler "\[ \t\]addb" } } */

struct S1 test_sub (struct S1 a, struct S2 b, struct S2 c)
{
  a.val = b.val - c.val;

  return a;
}

/* { dg-final { scan-assembler "\[ \t\]subb" } } */
