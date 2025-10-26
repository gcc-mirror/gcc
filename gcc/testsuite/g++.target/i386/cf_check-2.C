/* { dg-do compile { target { "i?86-*-* x86_64-*-*" } } } */
/* { dg-options "-O2 -fcf-protection" } */

extern void bar (void);
extern void bar (void) __attribute__((nocf_check)); /* { dg-error "ambiguating new declaration" } */
extern void foo (void) __attribute__((nocf_check));
extern void foo (void);

void
func (void)
{
  bar ();
  foo ();
}
