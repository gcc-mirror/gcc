/* { dg-options "-O2" } */
/* { dg-do run } */

static const long long int TagTypeNumber = 0xffff000000000000ll;

long long int x;

void foo(void)
{
  x = TagTypeNumber + 1;
}

int main(int argc, char **argv)
{
  if (argc > 0)
    foo ();

  if ((x & TagTypeNumber) == TagTypeNumber)
  {
    unsigned y = (unsigned)x;
    __builtin_printf ("v: %u\n", y);
    if (y != 1)
      __builtin_abort ();
  }

  return 0;
}
