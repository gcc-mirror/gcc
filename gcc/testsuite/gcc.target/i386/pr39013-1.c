/* PR target/39013 */
/* { dg-do compile } */
/* { dg-require-effective-target pie } */
/* { dg-options "-O2 -fpie -std=gnu89" } */

inline int foo (void);
extern inline int bar (void);

int
main (void)
{
  return foo () + bar ();
}

/* { dg-final { scan-assembler "foo@PLT" } } */
/* { dg-final { scan-assembler "bar@PLT" } } */
