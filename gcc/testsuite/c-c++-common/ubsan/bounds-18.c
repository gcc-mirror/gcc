/* PR sanitizer/108060 */
/* { dg-do run } */
/* { dg-options "-fsanitize=bounds" } */
/* { dg-skip-if "" { *-*-* } "-flto" } */
/* { dg-shouldfail "ubsan" } */

int a[8];
int c;

int
main ()
{
  int b = -32768;
  a[b] = a[b] | c;
}

/* { dg-output "index -32768 out of bounds for type 'int \\\[8\\\]'" } */
