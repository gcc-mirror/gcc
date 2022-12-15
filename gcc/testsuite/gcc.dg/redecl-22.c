/* We used to ICE in the gimplifier, PR 107705 */
/* { dg-do compile } */
/* { dg-options "-w" } */
int f (void)
{
  int (*p) (void) = 0; // { dg-note "" }
  return p ();
  int p = 1; // { dg-error "" }
}
