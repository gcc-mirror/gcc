/* { dg-lto-do run } */
/* { dg-lto-options { { -O2 -flto } } } */

__attribute__((weak)) int get_t(void)
{
  return 0;
}

int a;
int main(void)
{
  a = get_t();
  if (a != 1)
    __builtin_abort ();
  return 0;
}
