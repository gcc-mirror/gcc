/* Reduced from infoleak ICE seen on Linux kernel with
   -Wno-analyzer-use-of-uninitialized-value.

   Verify that we complain about the uninit value when
   -Wno-analyzer-use-of-uninitialized-value isn't supplied.  */

/* { dg-do compile } */
/* { dg-options "-fanalyzer" } */
/* { dg-require-effective-target analyzer } */

extern unsigned long
copy_to_user(void* to, const void* from, unsigned long n);

unsigned long
test_uninit_size (void *to, void *from)
{
  unsigned long n;
  char buf[16];
  return copy_to_user(to, from, n); /* { dg-warning "use of uninitialized value 'n'" } */
}
