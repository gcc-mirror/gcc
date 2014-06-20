/* { dg-do run } */
/* { dg-shouldfail "asan" } */

__attribute__((noinline,noclone)) void
foo (char *a, char *b) {
  a[0] = b[0] = 0;
  __builtin_memcpy(a, b, 4);
}

int
main () {
  char a, b;
  foo (&a, &b);
  return 0;
}

/* { dg-output "ERROR: AddressSanitizer: stack-buffer-overflow" } */
