/* { dg-do compile } */
/* { dg-options "-O1 -fharden-conditional-branches -funroll-loops --param max-loop-header-insns=1" } */

__attribute__ ((cold)) void
bar (void);

void
foo (int x)
{
  if (x)
    {
      int i;

      for (i = 0; i < 101; ++i)
        bar ();
    }
}
