/* { dg-options "-fno-lto" } */

extern int foo(void);

int main(void)
{
  return foo();
}
