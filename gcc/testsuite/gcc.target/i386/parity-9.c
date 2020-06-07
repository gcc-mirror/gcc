/* { dg-do compile } */
/* { dg-options "-O2 -march=core-avx2" } */
/* { dg-additional-options "-mregparm=1" { target ia32 } } */
/* { dg-final { scan-assembler-not "popcnt" } } */
/* { dg-final { scan-assembler-not "shr" } } */
/* { dg-final { scan-assembler-times "jp" 2 } } */
/* { dg-final { scan-assembler-times "jnp" 2 } } */

void dummy(void);

void pos8(unsigned char x)
{
  if (__builtin_parity(x))
    dummy();
}

void neg8(unsigned char x)
{
  if (!__builtin_parity(x))
    dummy();
}

void pos16(unsigned short x)
{
  if (__builtin_parity(x))
    dummy();
}

void neg16(unsigned short x)
{
  if (!__builtin_parity(x))
    dummy();
}
