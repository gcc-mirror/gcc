/* Origin: PR c/410 from Jan Echternach
   <jan.echternach@informatik.uni-rostock.de>,
   adapted to a testcase by Joseph Myers <jsm28@cam.ac.uk>.
*/

extern void exit (int);

static void
foo (void)
{
  struct {
    long a;
    char b[1];
  } x = { 2, { 0 } };
}

int
main (void)
{
  int tmp;
  foo ();
  tmp = 1;
  exit (0);
}
