/* { dg-do run } */
/* { dg-additional-options "-fallow-store-data-races" } */

const int a;
int *b;
int main()
{
  &a != b || (*b = 1);
  return 0;
}
