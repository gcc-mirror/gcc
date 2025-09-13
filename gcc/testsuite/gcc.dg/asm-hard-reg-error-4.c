/* { dg-do compile { target lra } } */

/* Verify output operands.  */

#if defined __hppa__
# define R0 "20"
# define R1 "21"
#elif defined __AVR__
# define R0 "20"
# define R1 "24"
#elif defined __PRU__
# define R0 "0"
# define R1 "4"
#else
# define R0 "0"
# define R1 "1"
#endif

int
test (void)
{
  int x;
  register int y __asm__ (R0);

  /* Preserve status quo and don't error out.  */
  __asm__ ("" : "=r" (x), "=r" (x));

  /* Be more strict for hard register constraints and error out.  */
  __asm__ ("" : "={"R0"}" (x), "={"R1"}" (x)); /* { dg-error "multiple outputs to lvalue 'x'" } */

  /* Still error out in case of a mixture.  */
  __asm__ ("" : "=r" (x), "={"R1"}" (x)); /* { dg-error "multiple outputs to lvalue 'x'" } */

  return x + y;
}
