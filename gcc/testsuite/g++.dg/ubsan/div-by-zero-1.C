/* { dg-do compile } */
/* { dg-options "-fsanitize=shift -w" } */

void
foo (int i)
{
  switch (i)
  case 0 * (1 / 0): /* { dg-error "is not a constant expression" } */
    ;
}
