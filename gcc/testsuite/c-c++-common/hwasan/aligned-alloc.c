/* { dg-do run } */
/* { dg-require-effective-target hwaddress_exec } */
/* { dg-shouldfail "hwasan" } */
/* This program fails at runtime in the libhwasan library.
   The allocator can't handle the requested invalid alignment.  */

int
main ()
{
  void *p = __builtin_aligned_alloc (17, 100);
  if (((unsigned long long)p & 0x10) == 0)
    return 0;
  return 1;
}

/* { dg-output "HWAddressSanitizer: invalid alignment requested in aligned_alloc: 17" } */
