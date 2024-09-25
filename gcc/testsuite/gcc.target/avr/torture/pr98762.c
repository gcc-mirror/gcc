/* { dg-do run } */
/* { dg-additional-options "-std=c99" } */

long long acc = 0x1122334455667788;

__attribute__((noipa))
void addhi (short a)
{
  acc += (long long) a << 32;
}

int main (void)
{
  addhi (0x0304);
  if (acc != 0x1122364855667788)
    __builtin_abort();

  return 0;
}
