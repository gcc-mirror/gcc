/* PR sanitizer/108894 */
/* { dg-do run } */
/* { dg-options "-fsanitize=bounds -fsanitize-recover=bounds" } */
/* { dg-output "index 1 out of bounds for type 'int \\\[\[*0-9x]*\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*index 0 out of bounds for type 'int \\\[\[*0-9x]*\\\]'" } */

struct S { int a; int b[0]; int c; } s;

int
main ()
{
  int *volatile p = &s.b[0];
  p = &s.b[1];
  int volatile q = s.b[0];
}
