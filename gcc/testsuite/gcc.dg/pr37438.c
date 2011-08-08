/* PR target/37438 */
/* { dg-do compile } */
/* { dg-options "-Os" } */
/* { dg-options "-Os -march=i486" { target { { i686-*-* x86_64-*-* } && ia32 } } } */

extern int bar (unsigned long long int);
extern int baz (const char *, unsigned int, unsigned short);

int
foo (unsigned long long int x)
{
  return (x & 0xff) | ((unsigned int) (x >> 12) & ~0xff);
}

int
test (const char *v, unsigned int w, unsigned long long int x)
{
  unsigned short k;
  k = ((bar (x) & 0xff) << 8) | (foo (x) & 0xff);
  return baz (v, w, k);
}
