// { dg-do compile }
// { dg-options "-O2 -fdump-tree-threadfull1-details" }

void stuff();

void foo (float a, int cond)
{
  float x;

  if (cond)
    x = a;
  else
    x = 8.0;

  /* We should be able to fold this as false on the path coming out of
     cond == TRUE conditional.  */
  if (x < a)
    stuff();
}

// { dg-final { scan-tree-dump "Registering jump thread: \\(2, 4\\)" "threadfull1" } }
