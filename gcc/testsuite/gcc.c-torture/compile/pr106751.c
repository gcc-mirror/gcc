/* PR rtl-optimization/106751 */

int *foo (void);

void
bar (void)
{
  asm goto ("" : : : : lab);
  __builtin_unreachable ();
lab:
  while (1)
    {
      int o;
      asm ("" : "=r" (o) : "g" (1));
      *foo () = o;
    }
}
