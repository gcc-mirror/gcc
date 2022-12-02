// PR c++/107504
// { dg-do compile { target c++17 } }
// { dg-additional-options -fdump-tree-gimple-lineno }
// { dg-final { scan-tree-dump-not {value-expr: \[} "gimple" } }

struct S
{
  void* i;
  int j;
};

S f(char* p)
{
  return {p, 1};
}

int main()
{
  char buf[1];
  auto [x, y] = f(buf);
  if (x != buf)
    throw 1;
  if (y != 1)
    throw 2;
  return 0;
}
