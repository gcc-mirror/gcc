/* { dg-do compile } */
/* { dg-options "-fcilkplus" } */

int main (int argc, char **argv)
{
  int x = 0;
  int array[10][10], array2[10];

  array[:][:] = array[:]; /* { dg-error "rank mismatch between" } */

  x = array2[:]; /* { dg-error "cannot be scalar when" } */

  return 0;
}
