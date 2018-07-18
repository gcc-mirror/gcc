/* PR target/78904 */
/* { dg-do compile } */
/* { dg-require-effective-target nonpic } */
/* { dg-options "-O2 -masm=att" } */

struct S1
{
  unsigned char pad1;
  unsigned char val;
  unsigned short pad2;
};

extern struct S1 t;

struct S1 test_and (struct S1 a)
{
  a.val &= t.val;

  return a;
}

/* { dg-final { scan-assembler "\[ \t\]andb\[ \t\]+t\[^\n\r]*, %.h" } } */

struct S1 test_or (struct S1 a)
{
  a.val |= t.val;

  return a;
}

/* { dg-final { scan-assembler "\[ \t\]orb\[ \t\]+t\[^\n\r]*, %.h" } } */

struct S1 test_xor (struct S1 a)
{
  a.val ^= t.val;

  return a;
}

/* { dg-final { scan-assembler "\[ \t\]xorb\[ \t\]+t\[^\n\r]*, %.h" } } */

struct S1 test_add (struct S1 a)
{
  a.val += t.val;

  return a;
}

/* { dg-final { scan-assembler "\[ \t\]addb\[ \t\]+t\[^\n\r]*, %.h" } } */
