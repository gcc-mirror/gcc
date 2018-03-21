/* PR target/82989.  */
/* { dg-do compile } */
/* { dg-require-effective-target arm_neon_ok } */
/* { dg-skip-if "avoid conflicts with multilib options" { *-*-* } { "-mcpu=*" } { "-mcpu=cortex-a8" } } */
/* { dg-skip-if "avoid conflicts with multilib options" { *-*-* } { "-mfpu=*" } { "-mfpu=neon" } } */
/* { dg-skip-if "avoid conflicts with multilib options" { *-*-* } { "-mfloat-abi=*" } { "-mfloat-abi=hard" } } */
/* { dg-options "-O2 -mcpu=cortex-a8 -mfpu=neon -mfloat-abi=hard" } */
/* { dg-add-options arm_neon } */

typedef unsigned long long uint64_t;

void f_shr_imm (uint64_t *a)
{
  *a += *a >> 32;
}

void f_shr_reg (uint64_t *a, uint64_t b)
{
  *a += *a >> b;
}

void f_shl_imm (uint64_t *a)
{
  *a += *a << 32;
}

void f_shl_reg (uint64_t *a, uint64_t b)
{
  *a += *a << b;
}
/* { dg-final { scan-assembler-not "vshl*" } } */
/* { dg-final { scan-assembler-not "vshr*" } } */
/* { dg-final { scan-assembler-not "vmov*" } } */
