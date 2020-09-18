/* { dg-options "-fno-early-inlining" } */
/* { dg-require-effective-target non_strict_prototype } */

void
bar ()
{
  bar (0);
}

__attribute__ ((flatten))
void foo ()
{
  bar ();
}
