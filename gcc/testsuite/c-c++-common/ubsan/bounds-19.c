/* PR sanitizer/108060 */
/* { dg-do run } */
/* { dg-options "-fsanitize=bounds" } */
/* { dg-skip-if "" { *-*-* } "-flto" } */
/* { dg-shouldfail "ubsan" } */

int a[8];
int a2[18];
int c;

int
main ()
{
  int b = 0;
  a[0] = (a2[b], b = -32768, a[0] | c);
  b = 0;
  a[b] = (a[b], b = -32768, a[0] | c);
}

/* { dg-output "index -32768 out of bounds for type 'int \\\[8\\\]'" } */
