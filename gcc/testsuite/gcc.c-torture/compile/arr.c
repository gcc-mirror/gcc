/* { dg-additional-options "-std=gnu89" } */

foo (a, b, c)
{
  bar (a, b);
  {
    int arr[10];
    arr[c] = b;
    bar (arr[0], arr[1]);
  }
}
