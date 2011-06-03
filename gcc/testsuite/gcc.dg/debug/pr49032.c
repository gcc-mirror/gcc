/* PR debug/49032 */
/* { dg-do link } */

static int s = 42;

int
main ()
{
  int *l[18] = { &s, &s, &s, &s, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
  return 0;
}
