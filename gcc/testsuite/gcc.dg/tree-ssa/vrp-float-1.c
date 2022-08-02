// { dg-do compile }
// { dg-options "-O2 -fdisable-tree-ethread -fdisable-tree-fre1 -fdump-tree-evrp-details" }

void bar ();
void george ();

float
foo (float x, float y)
{
  if (x == x)
    {
      if (x > y)
        bar();
      if (x == x)
        george();
    }
}

// { dg-final { scan-tree-dump-times "Folded into: if \\(1 != 0\\)" 1 "evrp" } }
