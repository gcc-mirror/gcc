/* { dg-do compile } */
/* { dg-additional-options "-march=znver5" { target { x86_64-*-* i?86-*-* } } } */

void fancy_abort(const char *, int, const char *);
int usage_insns_0_0;
void inherit_in_ebb() {
  int abis = 0;
  for (int i = 0; i < 8; ++i)
    if (i > usage_insns_0_0)
      abis |= i;
  abis ? fancy_abort("", 0, __FUNCTION__), 0 : 0;
}
