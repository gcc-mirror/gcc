/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2 -mregparm=3" } */

extern long a1, a2, a3;
extern void foo (void *, void *, void *);
void
bar (void *eax, void *edx, void *ecx)
{
  asm ("" : "=a"(a1) : "a"(0));
  asm ("" : "=d"(a2) : "d"(0));
  asm ("" : "=c"(a3) : "c"(0));
  foo (eax, edx, ecx);
}
