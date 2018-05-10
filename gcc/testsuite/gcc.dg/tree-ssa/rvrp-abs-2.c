/* { dg-do compile } */
/* { dg-options "-fwrapv -O2 -fno-tree-forwprop -fdisable-tree-ethread -fno-tree-fre -fdump-tree-rvrp" } */

/* With -fwrapv, ABS_EXPR(-MIN) is undefined.  Test that we're not
   making any assumptions on the absolute value of -MIN.  */

extern void do_not_abort ();
extern void keep ();

int g (int b) {
  if (b < -23 || b > 64)
    {
      // b = [-MIN, -24][65, +MAX]
      b = b > 0 ? b : -b;
      // b = [24, MAX]
      if (b == -20 || b == 20)
        do_not_abort ();
      if (b < 24)
        do_not_abort ();
      if (b < 25)
        keep();
    }
}

/* { dg-final { scan-tree-dump-times "do_not_abort" 2 "rvrp" } } */
/* { dg-final { scan-tree-dump-times "keep" 1 "rvrp" } } */
