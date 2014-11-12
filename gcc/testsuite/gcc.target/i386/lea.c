/* { dg-do compile } */
/* { dg-require-effective-target ia32 } */
/* { dg-skip-if "" { *-*-* } { "-march=*" } { "-march=pentiumpro" } } */
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
