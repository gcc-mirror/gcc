/* { dg-lto-do link } */

int _umh(int i)
{
  return i+1;
}

int weaks(int i) __attribute__((weak, alias("_umh")));

int main()
{
  return weaks(10);
}
