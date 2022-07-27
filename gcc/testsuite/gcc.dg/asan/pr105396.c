/* PR sanitizer/105396 */
/* { dg-do run } */
/* { dg-skip-if "" { *-*-* } { "*" } { "-O0" } } */
/* { dg-shouldfail "asan" } */

int
main ()
{
  int a;
  int *b[1];
  int c[10];
  int d[1][1];
  for (a = 0; a < 1; a++)
    d[1][a] = 0;
  return 0;
}

/* { dg-output "ERROR: AddressSanitizer: stack-buffer-overflow on address.*(\n|\r\n|\r)" } */
/* { dg-output "WRITE of size.*" } */
