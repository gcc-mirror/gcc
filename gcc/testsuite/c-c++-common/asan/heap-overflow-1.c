/* { dg-do run } */
/* { dg-options "-fno-builtin-malloc -fno-builtin-free -fno-builtin-memset" } */
/* { dg-shouldfail "asan" } */

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

/* { dg-output "READ of size 1 at 0x\[0-9a-f\]+ thread T0.*(\n|\r\n|\r)" } */
/* { dg-output "    #0 0x\[0-9a-f\]+ +(in _*main (\[^\n\r]*heap-overflow-1.c:21|\[^\n\r]*:0|\[^\n\r]*\\+0x\[0-9a-z\]*)|\[(\]).*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*0x\[0-9a-f\]+ is located 0 bytes to the right of 10-byte region\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*allocated by thread T0 here:\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "    #0 0x\[0-9a-f\]+ +(in _*(interceptor_|wrap_|)malloc|\[(\])\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "    #1 0x\[0-9a-f\]+ +(in _*main (\[^\n\r]*heap-overflow-1.c:19|\[^\n\r]*:0|\[^\n\r]*\\+0x\[0-9a-z\]*)|\[(\])\[^\n\r]*(\n|\r\n|\r)" } */
