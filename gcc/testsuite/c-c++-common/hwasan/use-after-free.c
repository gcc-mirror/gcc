/* { dg-do run } */
/* { dg-require-effective-target hwaddress_exec } */
/* { dg-options "-fno-builtin-malloc -fno-builtin-free" } */
/* { dg-shouldfail "hwasan" } */

#ifdef __cplusplus
extern "C" {
#endif
void *malloc (__SIZE_TYPE__);
void free (void *);
#ifdef __cplusplus
}
#endif

int main() {
  char *x = (char*)malloc(10);
  free(x);
  return x[5];
}


/* { dg-output "HWAddressSanitizer: tag-mismatch on address 0x\[0-9a-f\]*.*" } */
/* { dg-output "READ of size 1 at 0x\[0-9a-f\]* tags: \[\[:xdigit:\]\]\[\[:xdigit:\]\]/\[\[:xdigit:\]\]\[\[:xdigit:\]\] \\(ptr/mem\\) in thread T0.*" } */
/* { dg-output "is located 5 bytes inside a 10-byte region.*" } */
/* { dg-output "freed by thread T0 here:.*" } */
/* { dg-output "#1\[^\n\r]*main\[^\n\r]*use-after-free.c:17.*" } */
/* { dg-output "previously allocated by thread T0 here:.*" } */
/* { dg-output "#1\[^\n\r]*main\[^\n\r]*use-after-free.c:16" } */
