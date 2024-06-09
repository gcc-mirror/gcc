/* { dg-do run } */

#pragma pack(1)
struct A {
  int b : 4;
  int c : 11;
  int d : 2;
  int e : 5;
} f;
int main()
{
  struct A g = {1, 1, 1, 1};
  while (!g.b)
    f = g;
  return 0;
}
