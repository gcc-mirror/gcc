/* PR target/104775 */
/* { dg-do assemble { target s390_zEC12_hw } } */
/* { dg-options "-O2 -march=zEC12" } */

long a[64];
void bar (void);

void
foo (int x, int y)
{
  if (x != a[y])
    bar ();
  __builtin_trap ();
}
