/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -fomit-frame-pointer" } */

extern void boring(void);

extern void continuation(void *, void *, void *, void *)
  __attribute__((preserve_none));

__attribute__((preserve_none))
void entry(void *a, void *b, void *c, void *d)
{
  boring();
  continuation(a, b, c, d);
}

/* { dg-final { scan-assembler-not "movq" } } */
/* { dg-final { scan-assembler "jmp\[\\t \]+_?continuation" } } */
