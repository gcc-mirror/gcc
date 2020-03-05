/* PR target/39013 */
/* { dg-do compile } */
/* { dg-require-effective-target pie } */
/* { dg-options "-O2 -fpie -std=gnu99" } */

inline int foo (void);		/* { dg-warning "declared but never defined" } */
extern inline int bar (void);	/* { dg-warning "declared but never defined" } */

int
main (void)
{
  return foo () + bar ();
}

/* { dg-final { scan-assembler "foo@PLT" { target { ! *-*-darwin* } } } } */
/* { dg-final { scan-assembler "bar@PLT" { target { ! *-*-darwin* } } } } */

/* Darwin's dynamic linker does PIE without indirection.  */
/* { dg-final { scan-assembler {call[ \t]_foo} { target *-*-darwin* } } } */
/* { dg-final { scan-assembler {call[ \t]_bar} { target *-*-darwin* } } } */
