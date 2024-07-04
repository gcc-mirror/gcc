/* { dg-do compile } */
/* { dg-add-options vect_early_break } */
/* { dg-additional-options "-O3" } */

void test_sort_helper(int *);
int test_sort_driver_driver_real_last;
void test_sort_driver_driver(int start, int *e, int *f)
{
  for (int *l = e; l > f;)
    {
      *--l = start;
      if (f == l)
	test_sort_helper(&test_sort_driver_driver_real_last);
      if (start)
	test_sort_driver_driver(start - 1, e, f);
    }
}
