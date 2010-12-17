/* { dg-lto-do link } */

void baz(void)
{
  __builtin_abort ();
}
void foo(void)
{
  baz();
}
int main() { return 0; }
