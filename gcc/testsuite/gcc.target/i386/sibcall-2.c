/* { dg-do compile { xfail { *-*-* } } } */
/* { dg-require-effective-target ia32 } */
/* { dg-options "-O2" } */

extern int doo1 (int);
extern int doo2 (int);
extern void bar (char *);

int foo (int a)
{
  char s[256];
  bar (s);
  return (a < 0 ? doo1 : doo2) (a);
}

/* { dg-final { scan-assembler-not "call[ \t]*.%eax" } } */
