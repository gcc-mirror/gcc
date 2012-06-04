/* { dg-do run } */

extern void abort (void);

static volatile struct S0 {
    short f3[9];
    unsigned f8 : 15;
} s = {1};
static unsigned short sh = 0x1234;

struct S0 a, b;
int vi = 0;

void func_4()
{
  s.f8 |= 1;
  sh = 15;
  if (vi) a = b;
}

int main()
{
  func_4();
  if (sh != 15)
    abort ();
  return 0;
}
