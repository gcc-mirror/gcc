/* { dg-options "-fno-lto" } */

extern void foo(void);

int main(void)
{
  foo();
  return 0;
}
