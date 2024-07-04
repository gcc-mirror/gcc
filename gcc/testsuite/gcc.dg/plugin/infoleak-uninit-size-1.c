/* Reduced from infoleak ICE seen on Linux kernel with
   -Wno-analyzer-use-of-uninitialized-value.

   Verify that we don't ICE when complaining about an infoleak
   when the size is uninitialized.  */

/* { dg-do compile } */
/* { dg-options "-fanalyzer -Wno-analyzer-use-of-uninitialized-value" } */
/* { dg-require-effective-target analyzer } */

extern unsigned long
copy_to_user(void* to, const void* from, unsigned long n);

unsigned long
test_uninit_size (void *to, void *from)
{
  unsigned long n;
  char buf[16];
  return copy_to_user(to, from, n);
}
