/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-Og -fpic -fplt -mtls-dialect=gnu" } */

extern void func1 (long *);
extern int func2 (void);
extern void func3 (void);
static __thread long foo;
static __thread long bar;
long
func (void)
{
  func1 (&foo);
  func1 (&bar);
  if (func2 ())
    func3 ();
  return foo + bar;
}

/* { dg-final { scan-assembler-times "call\[ \t\]__tls_get_addr@PLT" 1 { target { ! ia32 } } } } */
