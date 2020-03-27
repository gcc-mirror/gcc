/* PR lto/94271 */
/* { dg-skip-if "" { ! { i?86-*-* x86_64-*-* } } } */
/* { dg-lto-do link } */
/* { dg-require-ifunc "" } */

int a;

static int __attribute__ ((target_clones ("default", "avx512f"))) fast_clamp ()
{}

void
c ()
{
  a = fast_clamp ();
}
