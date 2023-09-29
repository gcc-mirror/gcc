// { dg-do compile }
// { dg-options "-O2 -fgimple -fdump-tree-threadfull1-details" }

void stuff();

void __GIMPLE (ssa,startwith("threadfull1"))
foo (float a, float b, int cond)
{
  float x;

 __BB(2):
  if (cond_3(D) != 0)
    goto __BB4;
  else
    goto __BB3;

 __BB(3):
  goto __BB4;

  /* We should be able to thread BB2->BB4->BB5 even though we have no knowledge
     of the NANness of either x_1 or a_5.  */
 __BB(4):
  x_1 = __PHI (__BB2: a_5(D), __BB3: b_4(D));
  if (x_1 __UNEQ a_5(D))
    goto __BB5;
  else
    goto __BB6;

 __BB(5):
  stuff ();
  goto __BB6;

 __BB(6):
  return;

}

// { dg-final { scan-tree-dump "Registering jump thread: \\(2, 4\\)" "threadfull1" } }
