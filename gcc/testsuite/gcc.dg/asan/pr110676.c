/* PR sanitizer/110676 */
/* { dg-do run } */
/* { dg-skip-if "" { *-*-* } { "*" } { "-O0" } } */
/* { dg-shouldfail "asan" } */

int
main ()
{
  char s[1] = "A";
  return __builtin_strlen (s);
}

/* { dg-output "ERROR: AddressSanitizer: stack-buffer-overflow on address.*(\n|\r\n|\r)" } */
/* { dg-output "READ of size.*" } */
