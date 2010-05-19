/* { dg-lto-do link } */
/* { dg-skip-if "" { ! { i?86-*-linux* x86_64-*-linux* } } { "*" } { "" } } */
/* { dg-lto-options {{-fPIC -r -nostdlib -fwhopr -msse2}} } */
/* { dg-require-effective-target sse2 } */
/* { dg-suppress-ld-options {-fPIC -msse2} } */

typedef short v8hi __attribute__((__vector_size__(16)));
void func (void) {
  v8hi x, y, z;
  z = __builtin_ia32_paddw128 (x, y);
}
