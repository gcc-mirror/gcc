/* { dg-do run } */
/* { dg-options "-fsanitize=shift -w -std=c99" } */

int
main (void)
{
  int a = 1;
  a <<= 31;
}
/* { dg-output "left shift of 1 by 31 places cannot be represented in type 'int'" } */
