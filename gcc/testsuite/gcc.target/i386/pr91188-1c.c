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
test_and (struct S a)
{
  a.val &= 0x42;

  return a;
}

/* { dg-final { scan-assembler "\[ \t\]andb" } } */

struct S
test_or (struct S a)
{
  a.val |= 0x42;

  return a;
}

/* { dg-final { scan-assembler "\[ \t\]orb" } } */

struct S
test_xor (struct S a)
{
  a.val ^= 0x42;

  return a;
}

/* { dg-final { scan-assembler "\[ \t\]xorb" } } */

struct S
test_sal (struct S a)
{
  a.val <<= 3;

  return a;
}

/* { dg-final { scan-assembler "\[ \t\]salb" } } */

struct S
test_shr (struct S a)
{
  a.val >>= 3;

  return a;
}

/* { dg-final { scan-assembler "\[ \t\]shrb" } } */

struct S
test_sar (struct S a)
{
  a.val = (signed char) a.val >> 3;

  return a;
}

/* { dg-final { scan-assembler "\[ \t\]sarb" } } */

struct S
test_rol (struct S a)
{
  a.val = (a.val << 3 | a.val >> 5) ;

  return a;
}

/* { dg-final { scan-assembler "\[ \t\]rolb" } } */

struct S
test_ror (struct S a)
{
  a.val = (a.val >> 3 | a.val << 5) ;

  return a;
}

/* { dg-final { scan-assembler "\[ \t\]rorb" } } */

struct S
test_add (struct S a)
{
  a.val += 42;

  return a;
}

/* { dg-final { scan-assembler "\[ \t\]addb" } } */

struct S
test_sub (struct S a)
{
  a.val -= 42;

  return a;
}

/* { dg-final { scan-assembler "\[ \t\]subb" } } */
