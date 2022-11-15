/* { dg-do run } */
/* { dg-require-effective-target hwaddress_exec } */
/* { dg-options "-fno-builtin-malloc -fno-builtin-free -fno-builtin-memset" } */
/* { dg-shouldfail "hwasan" } */

#ifdef __cplusplus
extern "C" {
#endif
void *memset (void *, int, __SIZE_TYPE__);
void *malloc (__SIZE_TYPE__);
void free (void *);
#ifdef __cplusplus
}
#endif

volatile int ten = 10;
int main(int argc, char **argv) {
  char *x = (char*)malloc(10);
  memset(x, 0, 10);
  int res = x[ten];  /* BOOOM */
  free(x);
  return res;
}

/* { dg-output "HWAddressSanitizer: tag-mismatch on address 0x\[0-9a-f\]*.*" } */
/* { dg-output "READ of size 1 at 0x\[0-9a-f\]* tags: \[\[:xdigit:\]\]\[\[:xdigit:\]\]/\[\[:xdigit:\]\]\[\[:xdigit:\]\].* \\(ptr/mem\\) in thread T0.*" } */
/* { dg-output "located 0 bytes after 10-byte region.*" } */
/* { dg-output "allocated here:.*" } */
/* { dg-output "#1 0x\[0-9a-f\]+ +in _*main \[^\n\r]*heap-overflow.c:18" } */
