/* { dg-lto-do link } */
/* { dg-lto-options {{-flto -g -fvar-tracking-assignments}} } */
/* { dg-suppress-ld-options "-g -fvar-tracking-assignments" } */

void foo()
{
  int hex = 0x4;
}

int main()
{
  return 0;
}
