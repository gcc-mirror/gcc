/* { dg-do compile { target lra } } */

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

  __asm__ ("" : "={"R0"}{"R1"}" (x));            /* { dg-error "multiple hard register constraints in one alternative are not supported" } */
  __asm__ ("" : "={"R0"}m{"R1"}" (x) : "r" (x)); /* { dg-error "multiple hard register constraints in one alternative are not supported" } */
  __asm__ ("" : "={"R0"}m" (x) : "r" (x));
  __asm__ ("" : "=r" (x) : "{"R0"}{"R1"}" (x));  /* { dg-error "multiple hard register constraints in one alternative are not supported" } */
  __asm__ ("" : "=r" (x) : "{"R0"}i{"R1"}" (x)); /* { dg-error "multiple hard register constraints in one alternative are not supported" } */
  __asm__ ("" : "=r" (x) : "{"R0"}i" (x));

  __asm__ ("" : "={"R0"}r" (x) : "r" (x)); /* { dg-error "hard register constraints and regular register constraints in one alternative are not supported" } */
  __asm__ ("" : "=r{"R0"}" (x) : "r" (x)); /* { dg-error "hard register constraints and regular register constraints in one alternative are not supported" } */
  __asm__ ("" : "=r" (x) : "{"R0"}r" (x)); /* { dg-error "hard register constraints and regular register constraints in one alternative are not supported" } */
  __asm__ ("" : "=r" (x) : "r{"R0"}" (x)); /* { dg-error "hard register constraints and regular register constraints in one alternative are not supported" } */

  return x;
}
