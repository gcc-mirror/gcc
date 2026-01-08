/* { dg-do run } */

struct a {
    short b;
} c[7], e;
int d;
static struct a f[3][2] = {{}, {}, 5, 5};
void __attribute__((noipa)) g()
{
  for (; e.b >= 0; e.b--)
    {
      c[e.b + 6] = f[2][1];
      d = c[6].b;
    }
}
int main()
{
  g();
  if (d != 5)
    __builtin_abort ();
}
