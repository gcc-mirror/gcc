/* { dg-do run { target { ilp32 || lp64 } } } */
/* { dg-options "-O2" } */
/* { dg-shouldfail "asan" } */

struct S { int i; } __attribute__ ((packed));

__attribute__((noinline, noclone)) int
foo (struct S *s)
{
  return s->i;
}

__attribute__((noinline, noclone)) int
bar (int *s)
{
  return *s;
}

__attribute__((noinline, noclone)) struct S
baz (struct S *s)
{
  return *s;
}

int
main ()
{
  struct T { char a[3]; struct S b[3]; char c; } t;
  int v = 5;
  struct S *p = t.b;
  asm volatile ("" : "+rm" (p));
  p += 3;
  if (bar (&v) != 5) __builtin_abort ();
  volatile struct S w = baz (p);
  return 0;
}

/* { dg-output "ERROR: AddressSanitizer:\[^\n\r]*on address\[^\n\r]*" } */
/* { dg-output "0x\[0-9a-f\]+ at pc 0x\[0-9a-f\]+ bp 0x\[0-9a-f\]+ sp 0x\[0-9a-f\]+\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*READ of size 4 at 0x\[0-9a-f\]+ thread T0\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "    #0 0x\[0-9a-f\]+ (in _*baz(\[^\n\r]*misalign-2.c:22|\[^\n\r]*:0)|\[(\])\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "    #1 0x\[0-9a-f\]+ (in _*main (\[^\n\r]*misalign-2.c:34|\[^\n\r]*:0)|\[(\]).*(\n|\r\n|\r)" } */
