/* { dg-do run } */
/* { dg-options "-fno-tree-dce" } */

static inline int foo (int n, int k)
{
  struct S
  {
    int i[n];
    int value;
  } s[2];
  return s[k].value = 0;
}

int main ()
{
  return foo (2, 0);
}
