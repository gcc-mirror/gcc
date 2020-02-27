/* Manual reimplemenation of <cstdlib>, to test name-matching within std.  */

namespace std
{
  typedef __SIZE_TYPE__ size_t;
  void *malloc (std::size_t size);
  void *calloc (std::size_t num, std::size_t size);
  void free (void *ptr);
}

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
