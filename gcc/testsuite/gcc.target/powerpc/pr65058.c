/* { dg-do compile { target { powerpc*-*-aix* } } } */
/* { dg-final { scan-assembler "externfunc\\\[DS\\\]" } } */
/* { dg-final { scan-assembler "externvar\\\[UA\\\]" } } */

extern int externvar;
extern void externfunc(void);

int *localvar = &externvar;
void (*localfunc)(void) = externfunc;

