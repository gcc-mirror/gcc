/* { dg-do compile { target { ! { nvptx*-*-* visium-*-* } } } } */
/* { dg-options "-O0 -fpatchable-function-entry=1" } */
/* { dg-additional-options "-fno-pie" { target sparc*-*-* } } */

inline void
foo (void)
{
}

void
bar1 (void)
{
  foo ();
}
