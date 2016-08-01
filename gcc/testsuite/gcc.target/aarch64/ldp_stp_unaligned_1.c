/* { dg-options "-O2" } */

/* Check that we can use a REG + IMM addressing mode when moving an unaligned
   TImode value to and from memory.  */

struct foo
{
  long long b;
  __int128 a;
} __attribute__ ((packed));

void
bar (struct foo *p, struct foo *q)
{
  p->a = q->a;
}

/* { dg-final { scan-assembler-not "add\tx\[0-9\]+, x\[0-9\]+" } } */
/* { dg-final { scan-assembler-times "ldp\tx\[0-9\]+, x\[0-9\], .*8" 1 } } */
/* { dg-final { scan-assembler-times "stp\tx\[0-9\]+, x\[0-9\], .*8" 1 } } */
