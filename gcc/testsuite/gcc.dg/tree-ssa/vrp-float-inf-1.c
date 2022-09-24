// { dg-do compile }
// { dg-options "-O2 -fdump-tree-evrp-details" }

void foo ();
void bar (double);

void funky(double f, double g)
{
  if (f <= __builtin_inf ())
    foo ();
  else
    bar (f);
}

// { dg-final { scan-tree-dump-not " Inf,  Inf" "evrp" } }
