/* { dg-do compile { target { { aarch64*-*-* s390x-*-* } && int128 } } } */
/* { dg-options "-O2" } get rid of -ansi since we use __int128 */

/* Test register pairs.  */

#if defined (__aarch64__)
# define GPR1 "{x4}"
# define GPR2_RAW "x5"
#elif defined (__s390__)
# define GPR1 "{r4}"
# define GPR2_RAW "r5"
#endif

#define GPR2 "{"GPR2_RAW"}"

void
test (void)
{
  __asm__ ("" :: GPR1 ((__int128) 42));
  __asm__ ("" :: GPR2 ((__int128) 42)); /* { dg-error "register .* for operand 0 isn't suitable for data type" } */
  __asm__ ("" :: GPR1 ((__int128) 42), GPR2 (42)); /* { dg-error "multiple inputs to hard register" } */

  __int128 x;
  __asm__ ("" : "="GPR1 (x) :: GPR2_RAW); /* { dg-error "hard register constraint for output 0 conflicts with 'asm' clobber list" } */
  __asm__ ("" : "=r" (x) : GPR1 (x) : GPR2_RAW); /* { dg-error "hard register constraint for input 0 conflicts with 'asm' clobber list" } */
}
