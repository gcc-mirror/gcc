/* { dg-do compile } */
/* { dg-options "-O2" } */

typedef int v4si __attribute__ ((vector_size (16)));

void bar (void);
void
foo (void)
{
  v4si x = { 1, 1, 1, 1 };
  asm ("# %0" :: "w" (x));
  bar ();
  asm ("# %0" :: "w" (x));
}

/* { dg-final { scan-assembler-times {\tmovi\tv[0-9]+\.4s,} 2 } } */
/* { dg-final { scan-assembler-not {\tldr\t} } } */
/* { dg-final { scan-assembler-not {\tstr\t} } } */
