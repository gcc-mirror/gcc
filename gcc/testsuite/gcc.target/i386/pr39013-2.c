/* PR target/39013 */
/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2 -fpie -std=gnu99" } */

inline int foo (void);		/* { dg-warning "declared but never defined" } */
extern inline int bar (void);	/* { dg-warning "declared but never defined" } */

int
main (void)
{
  return foo () + bar ();
}

/* { dg-final { scan-assembler "foo@PLT" } } */
/* { dg-final { scan-assembler "bar@PLT" } } */
