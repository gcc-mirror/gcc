// { dg-do compile }
// { dg-options "-O2 -fdisable-tree-ethread -fdisable-tree-fre1 -fdump-tree-evrp" }

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

// { dg-final { scan-tree-dump-times "Folding predicate x_*to 1" "evrp" 1 } }
