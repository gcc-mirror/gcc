// { dg-do compile }
// { dg-options "-O2 -fdump-tree-all-stats" }

void stuff();

void foo (float a, int cond)
{
  float x;

  if (cond)
    x = a;
  else
    x = 8.0;

  /* Even though on the BB2->BB4 path, x_1 and a_4 are equivalent, we cannot
     fold x_1 == a_4 because of possible NANs:

    <bb 4>
    # x_1 = PHI <a_4(D)(2), 8.0e+0(3)>
    if (x_1 == a_4(D))
  */
  if (x == a)
    stuff();
}

// { dg-final { scan-tree-dump-not "Jumps threaded" "threadfull1" } }
// { dg-final { scan-tree-dump-not "Jumps threaded" "threadfull2" } }
