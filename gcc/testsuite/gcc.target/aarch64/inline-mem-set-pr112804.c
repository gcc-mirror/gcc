/* { dg-do compile } */
/* { dg-require-effective-target aarch64_mabi_ilp32 } */
/* { dg-options "-finline-stringops -mabi=ilp32 -Wno-deprecated -ftrivial-auto-var-init=zero" } */

short m(unsigned k) {
  const unsigned short *n[65];
  return 0;
}
