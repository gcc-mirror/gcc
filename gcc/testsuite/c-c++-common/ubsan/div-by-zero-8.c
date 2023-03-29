/* PR c/109151 */
/* { dg-do run } */
/* { dg-options "-fsanitize=integer-divide-by-zero -Wno-div-by-zero -fno-sanitize-recover=integer-divide-by-zero" } */
/* { dg-shouldfail "ubsan" } */

int d;

int
main ()
{
  d = ((short) (d == 1 | d > 9)) / 0;
}

/* { dg-output "division by zero" } */
