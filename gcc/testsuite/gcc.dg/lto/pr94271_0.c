/* PR lto/94271 */
/* { dg-lto-do link } */

int a;

static int __attribute__ ((target_clones ("default", "avx512f"))) fast_clamp ()
{}

void
c ()
{
  a = fast_clamp ();
}
