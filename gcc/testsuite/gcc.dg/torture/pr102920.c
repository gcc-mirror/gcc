/* { dg-do run } */
/* { dg-additional-options "-funswitch-loops" } */

unsigned short a = 42;
unsigned short b = 1;
long int c = 1;
unsigned char var_120;
unsigned char var_123;

void __attribute__((noipa)) test(unsigned short a, unsigned short b, long c)
{
  for (char i = 0; i < (char)c; i += 5)
    if (!b)
      var_120 = a;
    else
      var_123 = a;
}

int main()
{
  test(a, b, c);
  if (var_123 != 42)
    __builtin_abort ();
  return 0;
}
