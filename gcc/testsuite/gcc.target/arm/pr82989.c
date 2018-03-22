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
/* { dg-final { scan-assembler-not "vshr*" } } */

void f_shr_reg (uint64_t *a, uint64_t b)
{
  *a += *a >> b;
}
/* { dg-final { scan-assembler-not "vshl*" } } */
/* Only 2 times for f_shr_reg. f_shr_imm should not have any.  */
/* { dg-final { scan-assembler-times {lsr\tr[0-9]+, r[0-9]+, r[0-9]} 2 } } */

void f_shl_imm (uint64_t *a)
{
  *a += *a << 32;
}
/* { dg-final { scan-assembler-not "vshl*" } } */

void f_shl_reg (uint64_t *a, uint64_t b)
{
  *a += *a << b;
}
/* { dg-final { scan-assembler-not "vshl*" } } */
/* Only 2 times for f_shl_reg. f_shl_imm should not have any.  */
/* { dg-final { scan-assembler-times {lsl\tr[0-9]+, r[0-9]+, r[0-9]} 2 } } */
