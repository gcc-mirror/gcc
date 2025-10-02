/* { dg-do compile { target lra } } */

/* Test clobbers.
   See asm-hard-reg-error-{2,3}.c for tests involving register pairs.  */

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
  int x, y;
  __asm__ ("" : "={"R0"}" (x), "={"R1"}" (y) : : R1); /* { dg-error "hard register constraint for output 1 conflicts with 'asm' clobber list" } */
  __asm__ ("" : "={"R0"}" (x) : "{"R0"}" (y), "{"R1"}" (y) : R1); /* { dg-error "hard register constraint for input 1 conflicts with 'asm' clobber list" } */
  return x + y;
}
