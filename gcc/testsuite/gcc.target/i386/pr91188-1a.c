/* PR target/91188 */
/* { dg-do compile } */
/* { dg-options "-O2 -masm=att" } */
/* { dg-additional-options "-mregparm=3" { target ia32 } } */
/* { dg-final { scan-assembler-not "movzbl" } } */
/* { dg-final { scan-assembler-not "movb" } } */

struct S
{
  unsigned char val;
  unsigned char pad1;
  unsigned short pad2;
};

struct S
test_and (struct S a, unsigned char b)
{
  a.val &= b;

  return a;
}

/* { dg-final { scan-assembler "\[ \t\]andb" } } */

struct S
test_or (struct S a, unsigned char b)
{
  a.val |= b;

  return a;
}

/* { dg-final { scan-assembler "\[ \t\]orb" } } */

struct S
test_xor (struct S a, unsigned char b)
{
  a.val ^= b;

  return a;
}

/* { dg-final { scan-assembler "\[ \t\]xorb" } } */

struct S
test_add (struct S a, unsigned char b)
{
  a.val += b;

  return a;
}

/* { dg-final { scan-assembler "\[ \t\]addb" } } */

struct S
test_sub (struct S a, unsigned char b)
{
  a.val -= b;

  return a;
}

/* { dg-final { scan-assembler "\[ \t\]subb" } } */
