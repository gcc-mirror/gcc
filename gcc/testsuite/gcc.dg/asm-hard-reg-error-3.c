/* { dg-do compile { target arm*-*-* s390-*-* } } */
/* { dg-options "-std=c99" } we need long long */
/* { dg-additional-options "-mcpu=unset -march=armv7-a+fp -marm" { target arm*-*-* } } */

/* Test register pairs.  */

#if defined (__arm__)
# define GPR1 "{r4}"
# define GPR2_RAW "r5"
#elif defined (__s390__)
# define GPR1 "{r4}"
# define GPR2_RAW "r5"
#endif

#define GPR2 "{"GPR2_RAW"}"

void
test (void)
{
  __asm__ ("" :: GPR1 (42ll));
  __asm__ ("" :: GPR2 (42ll)); /* { dg-error "register .* for operand 0 isn't suitable for data type" } */
  __asm__ ("" :: GPR1 (42ll), GPR2 (42)); /* { dg-error "multiple inputs to hard register" } */

  long long x;
  __asm__ ("" : "="GPR1 (x) :: GPR2_RAW); /* { dg-error "hard register constraint for output 0 conflicts with 'asm' clobber list" } */
  __asm__ ("" : "=r" (x) : GPR1 (x) : GPR2_RAW); /* { dg-error "hard register constraint for input 0 conflicts with 'asm' clobber list" } */
}
