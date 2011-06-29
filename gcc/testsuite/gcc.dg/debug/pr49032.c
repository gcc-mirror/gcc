/* PR debug/49032 */
/* { dg-do link } */
/* Prune mips-tfile warning on alpha*-dec-osf* with -gcoff* -O*.  */
/* { dg-prune-output "warning, s not found in .*symbol tables" } */

static int s = 42;

int
main ()
{
  int *l[18] = { &s, &s, &s, &s, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
  return 0;
}
