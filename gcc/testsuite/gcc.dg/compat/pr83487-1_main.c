/* { dg-skip-if "no large alignment" { pdp11-*-* } } */

extern void do_test (void);

int
main ()
{
  do_test ();
  return 0;
}
