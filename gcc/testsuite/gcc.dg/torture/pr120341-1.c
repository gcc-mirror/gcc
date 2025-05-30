/* { dg-do run } */
/* { dg-additional-options "-fallow-store-data-races" } */

char a, *b;
int main()
{
  b = "0";
  if (a)
    b[0]++;
  return 0;
}
