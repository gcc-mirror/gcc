/* { dg-do compile } */
/* { dg-options "-O2 -mgeneral-regs-only -mno-cld" } */

typedef unsigned int uword_t __attribute__ ((mode (__word__)));

extern int error;

__attribute__((interrupt))
void
fn1 (void *p, short error_code)
{ /* { dg-error "interrupt service routine should have unsigned \(long long |long |\)int as the second argument" } */
}

__attribute__((interrupt))
void
fn2 (void)
{ /* { dg-error "interrupt service routine can only have a pointer argument and an optional integer argument" } */
}

__attribute__((interrupt))
void
fn3 (uword_t error_code)
{ /* { dg-error "interrupt service routine should have a pointer as the first argument" } */
  error = error_code;
}

__attribute__((interrupt))
void
fn4 (uword_t error_code, void *frame)
{ /* { dg-error "interrupt service routine should have .* the .* argument" } */
  error = error_code;
}

extern int fn5 (void *) __attribute__ ((interrupt)); /* { dg-error "interrupt service routine can't have non-void return value" } */

int
fn5 (void *frame)
{
  return 0;
}
