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

unsigned char a;

void
test_and (struct S b)
{
  a &= b.val;
}

/* { dg-final { scan-assembler "\[ \t\]andb" } } */

void
test_or (struct S b)
{
  a |= b.val;
}

/* { dg-final { scan-assembler "\[ \t\]orb" } } */

void
test_xor (struct S b)
{
  a ^= b.val;
}

/* { dg-final { scan-assembler "\[ \t\]xorb" } } */

void
test_add (struct S b)
{
  a += b.val;
}

/* { dg-final { scan-assembler "\[ \t\]addb" } } */

void
test_sub (struct S b)
{
  a -= b.val;
}

/* { dg-final { scan-assembler "\[ \t\]subb" } } */
