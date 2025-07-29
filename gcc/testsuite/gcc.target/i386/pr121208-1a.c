/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2 -fPIC -mno-sse -mno-mmx -mno-80387 -mtls-dialect=gnu" } */

extern __thread int bar;
extern void func (void);

__attribute__((no_caller_saved_registers))
void
foo (int error)
{
  bar = 1; /* { dg-error -mtls-dialect=gnu2 } */
  if (error == 0)
    func ();
  bar = 0;
}
