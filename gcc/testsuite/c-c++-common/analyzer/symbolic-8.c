/* Merger where "arr" has two different symbolic bindings.  */

void test (int i, int j, int flag)
{
  int arr[16];

  if (flag)
    arr[i] = 42;
  else
    arr[j] = 17;
}
