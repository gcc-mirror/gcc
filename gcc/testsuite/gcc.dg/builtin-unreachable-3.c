/* Check that a function containing only __builtin_unreachable()
   doesn't ICE.  */
/* { dg-do compile } */
/* { dg-options "-O2" } */
const char *
f (void)
{
  __builtin_unreachable ();
}
