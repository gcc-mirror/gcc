/* { dg-options "-fno-early-inlining" } */
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
