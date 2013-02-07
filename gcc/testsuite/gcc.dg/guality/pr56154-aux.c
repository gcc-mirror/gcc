/* PR debug/56154 */
/* { dg-do compile } */

extern void test_main (void);

int
main ()
{
  test_main ();
  return 0;
}
