// { dg-do compile { target c++2a } }
// { dg-additional-options -fdump-tree-gimple }
// { dg-final { scan-tree-dump-times "hot label" 5 "gimple" } }
// { dg-final { scan-tree-dump-times "cold label" 3 "gimple" } }

bool b;

template <class T> int f()
{
  if (b)
    [[likely]] return 0;
  else
    [[unlikely]] flabel: return 1;
  switch (b)
    {
      [[likely]] case true: break;
    };
  return 1;
}

int main()
{
  if (b)
    [[likely]] return 0;
  else if (b)
    [[unlikely]] elabel:
      return 1;
  else
    [[likely]] b = false;

  f<int>();

  switch (b)
    {
      [[likely]] case true: break;
      [[unlikely]] case false: break;
    };
}
