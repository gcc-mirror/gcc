// { dg-do compile }
// { dg-options "-O2 -fgimple -fdump-tree-evrp" }

void link_error();

void __GIMPLE (ssa,startwith("evrp"))
foo1 (float x, float y)
{
  __BB(2):
  if (x_4(D) >= y_5(D))
    goto __BB5;
  else
    goto __BB3;

  __BB(3):
  // Relation at this point is VREL_LT.
  if (x_4(D) __UNLT y_5(D))
    goto __BB5;
  else
    goto __BB4;

  __BB(4):
  link_error ();
  goto __BB5;

  __BB(5):
  return;
}

void __GIMPLE (ssa,startwith("evrp"))
foo2 (float x, float y)
{
  __BB(2):
  if (x_4(D) >= y_5(D))
    goto __BB5;
  else
    goto __BB3;

  __BB(3):
  // Relation at this point is VREL_LT.
  if (x_4(D) __UNLE y_5(D))
    goto __BB5;
  else
    goto __BB4;

  __BB(4):
  link_error ();
  goto __BB5;

  __BB(5):
  return;
}

// { dg-final { scan-tree-dump-not "link_error" "evrp" } }
