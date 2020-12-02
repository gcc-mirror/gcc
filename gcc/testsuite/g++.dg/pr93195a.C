/* { dg-do link { target { ! { nvptx*-*-* visium-*-* } } } } */
// { dg-require-effective-target o_flag_in_section }
/* { dg-options "-O0 -fpatchable-function-entry=1" } */
/* { dg-additional-options "-fno-pie" { target sparc*-*-* } } */
/* { dg-additional-sources pr93195b.C } */

extern void bar1 (void);

inline void
foo (void)
{
}

void
bar (void)
{
  foo ();
  bar1 ();
}

int
main ()
{
  bar ();
  return 0;
}

