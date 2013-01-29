/* { dg-do run } */
/* { dg-options "-fno-builtin-memset" } */
/* { dg-shouldfail "asan" } */

extern
#ifdef __cplusplus
"C"
#endif
void *memset (void *, int, __SIZE_TYPE__);

volatile int ten = 10;

int main() {
  char x[10];
  memset(x, 0, 10);
  int res = x[ten];  /* BOOOM */
  return res;
}

/* { dg-output "READ of size 1 at 0x\[0-9a-f\]+ thread T0\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "    #0 0x\[0-9a-f\]+ (in _*main (\[^\n\r]*stack-overflow-1.c:16|\[^\n\r]*:0)|\[(\]).*(\n|\r\n|\r)" } */
/* { dg-output "Address 0x\[0-9a-f\]+ is\[^\n\r]*frame <main>" } */
