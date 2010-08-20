/* PR rtl-optimization/45353 */
/* { dg-do compile } */
/* { dg-options "-O2 -fschedule-insns -fselective-scheduling" } */

void
foo ()
{
  __builtin_unreachable ();
}
