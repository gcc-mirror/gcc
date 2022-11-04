// Test that the ?: only creates one temporary.
// { dg-additional-options "-fdump-tree-gimple" }
// { dg-final { scan-tree-dump-times "struct A" 2 "gimple" } }

struct A
{
  int i;
  A(int);
};

bool b;
int main()
{
  A a = 1;
  a = b ? A(2) : A(3);
}
