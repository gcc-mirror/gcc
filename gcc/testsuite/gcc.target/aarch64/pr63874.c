/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-skip-if "Not applicable for mcmodel=large" { aarch64*-*-* }  { "-mcmodel=large" } { "" } } */

extern void __attribute__((weak)) foo_weakref (void);
void __attribute__((weak, noinline)) bar (void)
{
 return;
}
void (*f) (void);
void (*g) (void);

int
main (void)
{
 f = &foo_weakref;
 g = &bar;
 return 0;
}

/* { dg-final { scan-assembler-not "adr*foo_weakref" } } */
/* { dg-final { scan-assembler-not "\\.(word|xword)\tbar" } } */
