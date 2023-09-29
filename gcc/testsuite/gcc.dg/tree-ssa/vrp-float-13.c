// { dg-do compile }
// { dg-options "-O2 -fno-thread-jumps -fdisable-tree-fre1 -fdump-tree-evrp-details" }

void a(float, float);
void b(float, float);

void foo(float x, float y)
{
  if (x != y)
    a (x,y);
  else if (x < y)
    b (x,y);
}

// Test that the false side of if(x != y) has a range for y.
// { dg-final { scan-tree-dump "2->4  \\(F\\) y_3\\(D\\)" "evrp" } }
