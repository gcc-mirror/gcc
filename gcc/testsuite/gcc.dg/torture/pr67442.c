/* { dg-do run } */

short foo[100];

int main()
{
  short* bar = &foo[50];
  short i = 1;
  short j = 1;
  short value = bar[8 - i * 2 * j];
  return value;
}
