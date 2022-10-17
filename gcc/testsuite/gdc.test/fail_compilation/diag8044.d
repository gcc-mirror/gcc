/*
TEST_OUTPUT:
---
fail_compilation/diag8044.d(18): Error: template instance `diag8044.test!(Enum.Bar)` does not match template declaration `test(Enum en)()`
  with `en = Bar`
  must satisfy the following constraint:
`       0`
---
 */
enum Enum { Foo, Bar }
void test(Enum en)()
    if(0)
{
}

void main()
{
    test!(Enum.Bar)();
}
