/* { dg-lto-do link } */
/* { dg-skip-if "" { ! { i?86-*-linux* i?86-*-gnu* x86_64-*-linux* } } } */
/* { dg-lto-options {{-fPIC -r -nostdlib -flto -flto-partition=1to1 -msse2}} } */
/* { dg-require-effective-target sse2 } */
/* { dg-suppress-ld-options {-fPIC -msse2} } */
/* { dg-extra-ld-options "-flinker-output=nolto-rel" } */

typedef short v8hi __attribute__((__vector_size__(16)));
void func (void) {
  v8hi x, y, z;
  z = __builtin_ia32_paddw128 (x, y);
}
