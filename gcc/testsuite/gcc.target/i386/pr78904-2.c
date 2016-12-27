/* PR target/78904 */
/* { dg-do compile } */
/* { dg-options "-O2 -masm=att" } */

struct S1
{
  unsigned char pad1;
  unsigned char val;
  unsigned short pad2;
};

extern struct S1 t;

struct S1 test_and (struct S1 a, struct S1 b)
{
  a.val &= b.val;

  return a;
}

/* { dg-final { scan-assembler "\[ \t\]andb\[^\n\r]*, %.h" } } */

struct S1 test_or (struct S1 a, struct S1 b)
{
  a.val |= b.val;

  return a;
}

/* { dg-final { scan-assembler "\[ \t\]orb\[^\n\r]*, %.h" } } */

struct S1 test_xor (struct S1 a, struct S1 b)
{
  a.val ^= b.val;

  return a;
}

/* { dg-final { scan-assembler "\[ \t\]xorb\[^\n\r]*, %.h" } } */

struct S1 test_add (struct S1 a, struct S1 b)
{
  a.val += t.val;

  return a;
}

/* { dg-final { scan-assembler "\[ \t\]addb\[^\n\r]*, %.h" } } */
