/* { dg-do run } */
/* { dg-require-effective-target hwaddress_exec } */
/* { dg-shouldfail "hwasan" } */
/* { dg-skip-if "" { *-*-* }  { "-O0" } { "" } } */
/* { dg-additional-options "-fdump-tree-asan1 -save-temps" } */

/* Here to check that the ASAN_POISON stuff works just fine.
   This mechanism isn't very often used, but I should at least go through the
   code-path once in my testfile.  */
int
main ()
{
  int *ptr = 0;

  {
    int a;
    ptr = &a;
    *ptr = 12345;
  }

  return *ptr;
}

/* { dg-final { scan-tree-dump-times "ASAN_POISON" 1 "asan1" }  } */
/* { dg-final { scan-assembler-times "bl\\s*__hwasan_tag_mismatch4" 1 } } */
/* { dg-output "HWAddressSanitizer: tag-mismatch on address 0x\[0-9a-f\]*.*" } */
/* { dg-output "READ of size 4 at 0x\[0-9a-f\]* tags: \[\[:xdigit:\]\]\[\[:xdigit:\]\]/00 \\(ptr/mem\\) in thread T0.*" } */
/* { dg-output "Address 0x\[0-9a-f\]* is located in stack of thread T0.*" } */
/* { dg-output "SUMMARY: HWAddressSanitizer: tag-mismatch \[^\n\]*.*" } */
