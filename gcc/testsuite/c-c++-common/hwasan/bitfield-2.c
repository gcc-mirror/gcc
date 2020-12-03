/* { dg-do run } */
/* { dg-require-effective-target hwaddress_exec } */
/* { dg-shouldfail "hwasan" } */

/* Ensure that hwasan instruments bitfield accesses.  */
struct A
{
  /* Ensure the offset from the start of this struct to the bitfield we access
     is large enough to be in a different tag.  */
  char base[16];
  int : 4;
  long x : 7;
};

int __attribute__ ((noinline, noclone))
f (void *p) {
  return ((struct A *)p)->x;
}

int
main ()
{
  char a = 0;
  return f (&a);
}

/* { dg-output "HWAddressSanitizer: tag-mismatch on address 0x\[0-9a-f\]*.*" } */
/* { dg-output "READ of size 2 at 0x\[0-9a-f\]* tags: \[\[:xdigit:\]\]\[\[:xdigit:\]\]/00 \\(ptr/mem\\) in thread T0.*" } */
/* { dg-output "Address 0x\[0-9a-f\]* is located in stack of thread T0.*" } */
/* { dg-output "SUMMARY: HWAddressSanitizer: tag-mismatch \[^\n\]*.*" } */
