/* PR gcov-profile/94029 */
/* { dg-options "-ftest-coverage" } */
/* { dg-do compile } */

#define impl_test(name) void test_##name() { }
impl_test(t1
) impl_test(t2)

int main()
{
  return 0;
}

/* { dg-final { run-gcov remove-gcda gcov-pr94029.c } } */
