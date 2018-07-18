/* { dg-do run } */
/* { dg-options "-fno-omit-frame-pointer -fno-shrink-wrap" } */
/* { dg-additional-options "-mno-omit-leaf-frame-pointer" { target { i?86-*-* x86_64-*-* } } } */
/* { dg-shouldfail "asan" } */

__attribute__((noinline, noclone))
static void
NullDeref(int *ptr)
{
  ptr[10]++;
}

int main()
{
  NullDeref((int*)0);
  return 0;
}

/* { dg-output "ERROR: AddressSanitizer:? SEGV on unknown address\[^\n\r]*" } */
/* { dg-output "0x\[0-9a-f\]+ \[^\n\r]*pc 0x\[0-9a-f\]+.*(\n|\r\n|\r)" } */
/* { dg-output "    #0 0x\[0-9a-f\]+ +(in \[^\n\r]*NullDeref\[^\n\r]* (\[^\n\r]*null-deref-1.c:10|\[^\n\r]*:0|\[^\n\r]*\\+0x\[0-9a-z\]*)|\[(\])\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "    #1 0x\[0-9a-f\]+ +(in _*main (\[^\n\r]*null-deref-1.c:15|\[^\n\r]*:0|\[^\n\r]*\\+0x\[0-9a-z\]*)|\[(\])\[^\n\r]*(\n|\r\n|\r)" } */
