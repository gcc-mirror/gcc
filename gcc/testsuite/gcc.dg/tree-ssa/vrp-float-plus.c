// { dg-do compile }
// { dg-options "-O2 -fno-tree-fre -fno-tree-dominator-opts -fno-thread-jumps -fdump-tree-vrp2" }

double BG_SplineLength ()
{
  double lastPoint;
  double i;

  for (i = 0.01;i<=1;i+=0.1f)
    if (!(i != 0.0))
      {
        lastPoint = i;
      }
    else
      {
        lastPoint = 2;
      }
  return lastPoint;
}

// { dg-final { scan-tree-dump-times "return 2\\.0e" 1 "vrp2" } }
