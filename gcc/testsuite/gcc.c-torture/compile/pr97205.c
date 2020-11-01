int a;
typedef __attribute__((aligned(2))) int x;
int f ()
{
  x b = a;
  return b;
}
