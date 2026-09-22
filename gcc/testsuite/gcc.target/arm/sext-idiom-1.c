/* { dg-do compile } */
/* { dg-require-effective-target arm_arch_v6_ok } */
/* { dg-options "-O2" } */
/* { dg-add-options arm_arch_v6 } */
/* { dg-final { check-function-bodies "**" "" } } */

typedef unsigned short word;

/*
** cond_spelling:
** 	sxtb	r0, r0
** 	bx	lr
*/
int
cond_spelling (unsigned char c)
{
  unsigned e = c;
  if (e & 0x80)
    e -= 0x100;
  return (int) e;
}

/*
** xor_spelling:
** 	sxtb	r0, r0
** 	bx	lr
*/
int
xor_spelling (unsigned char x)
{
  return (int) ((x ^ 0x80u) - 0x80u);
}

/* The 6502 relative-branch shape: the sign extension folds into the load,
   so neither the mask nor the subtract survives.  */
word
relative (unsigned char *mem, word pc)
{
  unsigned ea = mem[pc++];
  if (ea & 0x80)
    ea -= 0x100;
  return (word) (pc + ea);
}

/* { dg-final { scan-assembler-times "ldrsb" 1 } } */
/* { dg-final { scan-assembler-not "#256" } } */
/* { dg-final { scan-assembler-not "eor" } } */
