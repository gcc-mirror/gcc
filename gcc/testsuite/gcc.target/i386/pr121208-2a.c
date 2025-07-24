/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2 -fPIC -mtls-dialect=gnu" } */

typedef unsigned int uword_t __attribute__ ((mode (__word__)));
extern __thread int bar;
extern void func (void);

__attribute__((target("general-regs-only")))
__attribute__((interrupt))
void
foo (void *frame, uword_t error)
{
  bar = 1; /* { dg-error -mtls-dialect=gnu2 } */
  if (error == 0)
    func ();
  bar = 0;
}
