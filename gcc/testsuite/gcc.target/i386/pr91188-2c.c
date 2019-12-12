/* PR target/91188 */
/* { dg-do compile } */
/* { dg-options "-O2 -masm=att" } */
/* { dg-additional-options "-mregparm=3" { target ia32 } } */
/* { dg-final { scan-assembler-not "movzwl" } } */
/* { dg-final { scan-assembler-not "movw" } } */

struct S
{
  unsigned short val;
  unsigned short pad;
};

struct S
test_and (struct S a)
{
  a.val &= 0x42;

  return a;
}

/* { dg-final { scan-assembler "\[ \t\]andw" } } */

struct S
test_or (struct S a)
{
  a.val |= 0x42;

  return a;
}

/* { dg-final { scan-assembler "\[ \t\]orw" } } */

struct S
test_xor (struct S a)
{
  a.val ^= 0x42;

  return a;
}

/* { dg-final { scan-assembler "\[ \t\]xorw" } } */

struct S
test_sal (struct S a)
{
  a.val <<= 3;

  return a;
}

/* { dg-final { scan-assembler "\[ \t\]salw" } } */

struct S
test_shr (struct S a)
{
  a.val >>= 3;

  return a;
}

/* { dg-final { scan-assembler "\[ \t\]shrw" } } */

struct S
test_sar (struct S a)
{
  a.val = (signed short) a.val >> 3;

  return a;
}

/* { dg-final { scan-assembler "\[ \t\]sarw" } } */

struct S
test_rol (struct S a)
{
  a.val = (a.val << 3 | a.val >> 13) ;

  return a;
}

/* { dg-final { scan-assembler "\[ \t\]rolw" } } */

struct S
test_ror (struct S a)
{
  a.val = (a.val >> 3 | a.val << 13) ;

  return a;
}

/* { dg-final { scan-assembler "\[ \t\]rorw" } } */

struct S
test_add (struct S a)
{
  a.val += 42;

  return a;
}

/* { dg-final { scan-assembler "\[ \t\]addw" } } */

struct S
test_sub (struct S a)
{
  a.val -= 42;

  return a;
}

/* { dg-final { scan-assembler "\[ \t\]subw" } } */
