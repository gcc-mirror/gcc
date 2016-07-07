/* PR middle-end/71494 */

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
