/* PR target/96939 */
/* { dg-lto-do link } */
/* { dg-require-effective-target arm_arch_v8a_link } */
/* { dg-lto-options { { -flto -O2 -mcpu=unset -march=armv8-a+simd -mfpu=auto} } } */

extern unsigned crc (unsigned, const void *);
typedef unsigned (*fnptr) (unsigned, const void *);
volatile fnptr fn;

int
main ()
{
  fn = crc;
  return 0;
}
