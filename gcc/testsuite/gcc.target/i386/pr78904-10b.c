/* PR target/78904 */
/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -masm=att" } */
/* { dg-final { scan-assembler-not "shr" } } */

struct S1
{
  unsigned char pad1;
  unsigned char val;
  unsigned short pad2;
  unsigned int pad3;
};

char test_and (struct S1 a, struct S1 b)
{
  return a.val & b.val;
}

/* { dg-final { scan-assembler "\[ \t\]andb" } } */

char test_or (struct S1 a, struct S1 b)
{
  return a.val | b.val;
}

/* { dg-final { scan-assembler "\[ \t\]orb" } } */

char test_xor (struct S1 a, struct S1 b)
{
  return a.val ^ b.val;
}

/* { dg-final { scan-assembler "\[ \t\]xorb" } } */

char test_add (struct S1 a, struct S1 b)
{
  return a.val + b.val;
}

/* { dg-final { scan-assembler "\[ \t\]addb" } } */

char test_sub (struct S1 a, struct S1 b)
{
  return a.val - b.val;
}

/* { dg-final { scan-assembler "\[ \t\]subb" } } */
