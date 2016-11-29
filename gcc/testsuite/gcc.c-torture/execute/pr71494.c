/* PR middle-end/71494 */
/* { dg-require-effective-target label_values } */

int
main ()
{
  void *label = &&out;
  int i = 0;
  void test (void)
  {
    label = &&out2;
    goto *label;
   out2:;
    i++;
  }
  goto *label;
 out:
  i += 2;
  test ();
  if (i != 3)
    __builtin_abort ();
  return 0;
}
