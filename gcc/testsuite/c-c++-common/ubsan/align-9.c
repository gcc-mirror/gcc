/* Limit this to known non-strict alignment targets.  */
/* { dg-do run { target { i?86-*-linux* x86_64-*-linux* } } } */
/* { dg-options "-O2 -fsanitize=alignment -fsanitize-recover=alignment" } */

__attribute__((noinline, noclone)) void
foo (void *p, const void *q)
{
  *(long int *) p = *(const long int *) q;
}

int
main ()
{
  struct S { long c; char f[64]; char d; char e[2 * sizeof (long)]; char g[64]; } s;
  __builtin_memset (&s, '\0', sizeof s);
  foo (&s.e[0], &s.e[sizeof (long)]);
  return 0;
}

/* { dg-output "\.c:8:\[0-9]*: \[^\n\r]*load of misaligned address 0x\[0-9a-fA-F]* for type 'const long int', which requires \[48] byte alignment.*" } */
/* { dg-output "\.c:8:\[0-9]*: \[^\n\r]*store to misaligned address 0x\[0-9a-fA-F]* for type 'long int', which requires \[48] byte alignment" } */
