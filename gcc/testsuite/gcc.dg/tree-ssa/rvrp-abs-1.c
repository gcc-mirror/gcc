/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-forwprop -fdisable-tree-ethread -fno-tree-fre -fdump-tree-rvrp" } */

extern void abort ();
extern void keep ();

int g (int b) {
  if (b < -23 || b > 64)
    {
      // b = [-MIN, -24][65, +MAX]
      b = b > 0 ? b : -b;
      // b = [24, MAX]
      if (b == -20 || b == 20)
        abort ();
      if (b < 24)
        abort ();
      if (b < 25)
        keep();
    }
  else
    {
      // b == [-23, 64]
      b = b > 0 ? b : -b;
      // b == [0, 64]
      if (b < 0 || b > 64)
        abort ();
      if (b >= 0 && b < 65)
        keep ();
    }
}

/* { dg-final { scan-tree-dump-times "abort" 0 "rvrp" } } */
/* { dg-final { scan-tree-dump-times "keep" 2 "rvrp" } } */
