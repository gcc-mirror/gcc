/* { dg-do compile } */
/* { dg-options "-mbranch-protection=pac-ret+leaf+b-key" } */

/* The correct pauth instruction should be generated no matter the return
  address signing key/scope specified in the options.  */

int foo() {
  /* { dg-final { scan-assembler-times "pacia1716" 1 } } */
  __builtin_aarch64_pacia1716(0, 0);
  /* { dg-final { scan-assembler-times "pacib1716" 1 } } */
  __builtin_aarch64_pacib1716(0, 0);
  /* { dg-final { scan-assembler-times "autia1716" 1 } } */
  __builtin_aarch64_autia1716(0, 0);
  /* { dg-final { scan-assembler-times "autib1716" 1 } } */
  __builtin_aarch64_autib1716(0, 0);
}
