#include <cstdlib>

void test_1 (void *ptr)
{
  std::free (ptr); /* { dg-message "first 'free' here" } */
  std::free (ptr); /* { dg-warning "double-'free' of 'ptr'" } */
}

void test_2 (void)
{
  void *p = std::malloc (1024); /* { dg-message "allocated here" } */
} /* { dg-warning "leak of 'p'" } */

void test_3 (void)
{
  void *p = std::calloc (42, 1024); /* { dg-message "allocated here" } */
} /* { dg-warning "leak of 'p'" } */
