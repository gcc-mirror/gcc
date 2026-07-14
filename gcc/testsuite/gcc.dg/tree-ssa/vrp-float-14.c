// { dg-do compile }
// { dg-options "-O2 -fgimple -fdump-tree-evrp" }

int g;

int __GIMPLE (ssa, startwith ("evrp"))
some_constant (double x)
{
  __BB(2):
  if (x_1(D) != 1.0e+0)
    goto __BB3;
  else
    goto __BB5;

  __BB(3):
  g = 1;
  goto __BB4;

  __BB(4):
  if (x_1(D) == 1.0e+0)		// should fold to false
    goto __BB5;
  else
    goto __BB6;

  __BB(5):
  return 0;

  __BB(6):
  return 1;
}

// { dg-final { scan-tree-dump-not "if \\(x_1\\(D\\) == 1\\.0e\\+0\\)" "evrp" } }
