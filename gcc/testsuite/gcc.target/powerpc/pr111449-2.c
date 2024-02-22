/* { dg-do compile { target { has_arch_pwr8 } } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mvsx -O2" } */

/* Ensure 16-byte by pieces move is enabled.  */

void move1 (void *s1, void *s2)
{
  __builtin_memcpy (s1, s2, 16);
}

void move2 (void *s1)
{
  __builtin_memcpy (s1, "0123456789012345", 16);
}

/* { dg-final { scan-assembler-times {\mlxvd2x\M|\mp?lxv\M} 2 } } */
/* { dg-final { scan-assembler-times {\mstxvd2x\M|\mstxv\M} 2 } } */
