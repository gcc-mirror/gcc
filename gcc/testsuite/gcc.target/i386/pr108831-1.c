/* PR target/108831 */
/* { dg-do compile } */
/* { dg-options "-O2 -masm=att" } */
/* { dg-additional-options "-mregparm=3" { target ia32 } } */
/* { dg-final { scan-assembler-not "movzbl" } } */
/* { dg-final { scan-assembler-not "movb" } } */

struct S
{
  unsigned char pad1;
  unsigned char val;
  unsigned short pad2;
};

unsigned char
test_and (unsigned char a, struct S b)
{
  a &= b.val;

  return a;
}

/* { dg-final { scan-assembler "\[ \t\]andb" } } */

unsigned char
test_or (unsigned char a, struct S b)
{
  a |= b.val;

  return a;
}

/* { dg-final { scan-assembler "\[ \t\]orb" } } */

unsigned char
test_xor (unsigned char a, struct S b)
{
  a ^= b.val;

  return a;
}

/* { dg-final { scan-assembler "\[ \t\]xorb" } } */

unsigned char
test_add (unsigned char a, struct S b)
{
  a += b.val;

  return a;
}

/* { dg-final { scan-assembler "\[ \t\]addb" } } */

unsigned char
test_sub (unsigned char a, struct S b)
{
  a -= b.val;

  return a;
}

/* { dg-final { scan-assembler "\[ \t\]subb" } } */
