/* PR target/118561: ICE with -mfpu=none */
/* { dg-do compile } */
/* { dg-options "-O2 -march=loongarch64 -mfpu=none" } */

int
test (void)
{
  return __builtin_loongarch_movfcsr2gr (0); /* { dg-error "built-in function '__builtin_loongarch_movfcsr2gr' is not enabled" } */
}
