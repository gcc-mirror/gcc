// A store through the this parameter can throw a non-call exception even when
// the function has no local exception handler.

// { dg-do compile }
// { dg-options "-O2 -fnon-call-exceptions -fdump-tree-lim2-details" }

struct S
{
  int value;
  void loop (int);
};

void
S::loop (int n)
{
  for (int i = 0; i < n; ++i)
    value = i;
}

// { dg-final { scan-tree-dump-not "Executing store motion" "lim2" } }
