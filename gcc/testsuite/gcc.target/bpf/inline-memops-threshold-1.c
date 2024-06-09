
/* { dg-do compile } */
/* { dg-options "-O2 -masm=normal -minline-memops-threshold=256" } */

char buf[512];

void
mov_small (void)
{
  __builtin_memmove (buf, buf + 2, 255);
}

/* { dg-final { scan-assembler-not "call" } } */
/* { dg-final { scan-assembler "ldxb" } } */
/* { dg-final { scan-assembler "stxb" } } */
