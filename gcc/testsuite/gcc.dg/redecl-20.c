/* We used to ICE in the gimplifier, PR 107307 */
// { dg-do compile }
// { dg-options "-w" }
void f ()
{
  const struct { int a[1]; } b; // { dg-note "" }
  int *c = b.a;
  int *b; // { dg-error "" }
}
