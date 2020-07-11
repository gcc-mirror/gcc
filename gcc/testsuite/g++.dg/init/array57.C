// PR c++/78572
// { dg-do run { target c++11 } }

static int arr[10] = { arr[3]=5, arr[7]=3, };

struct X { int v; };
static X arr2[10] = { arr2[3]={5}, arr2[7]={3}, };

int
main()
{
  const int expected[10] = {5,3,0,5,0,0,0,3,0,0};
  for (int i = 0; i < 10; i++)
    if (arr[i] != expected[i] || arr2[i].v != expected[i])
      __builtin_abort();
}
