/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-O2 -march=pentiumpro" } */
/* { dg-final { scan-assembler "leal" } } */
typedef struct {
  char **visbuf;
  char **allbuf;
} TScreen;

void
VTallocbuf(TScreen *screen, unsigned long savelines)
{
  screen->visbuf = &screen->allbuf[savelines];
}
