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
  static char XXX[10];
  static char YYY[10];
  static char ZZZ[10];
  memset(XXX, 0, 10);
  memset(YYY, 0, 10);
  memset(ZZZ, 0, 10);
  int res = YYY[ten];  /* BOOOM */
  res += XXX[ten/10] + ZZZ[ten/10];
  return res;
}

/* { dg-output "READ of size 1 at 0x\[0-9a-f\]+ thread T0.*(\n|\r\n|\r)" } */
/* { dg-output "    #0 0x\[0-9a-f\]+ (in _*main (\[^\n\r]*global-overflow-1.c:20|\[^\n\r]*:0)|\[(\])\[^\n\r]*(\n|\r\n|\r).*" } */
/* { dg-output "0x\[0-9a-f\]+ is located 0 bytes to the right of global variable" } */
/* { dg-output ".*YYY\[^\n\r]* of size 10\[^\n\r]*(\n|\r\n|\r)" } */
