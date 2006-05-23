/* PR target/27696
   The testcase below uses to trigger an ICE.  */

/* { dg-do compile } */
/* { dg-options "-msse3" } */

void
foo (void const * P, unsigned int E, unsigned int H)
{
  __builtin_ia32_monitor (P, E, H);
}
