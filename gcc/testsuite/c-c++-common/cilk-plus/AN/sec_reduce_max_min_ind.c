/* { dg-do compile } */
/* { dg-options "-fcilkplus" } */

int main2 (int argc, char **argv);
int main (int argc, char **argv)
{
  int x = 0;
  if (argc == 1)
    {
      const char *array[] = {"a.out", "5"};
      x = main2 (2, (char **)array);
    }
  else
    x = main2 (argc, argv);

  return x;
}

int main2 (int argc, char **argv)
{
  int array[10][10], array2[10];
  int x, y;
  x = __sec_reduce_max_ind (array[:][:]); /* { dg-error "cannot have arrays with dimension greater than" } */

  y = __sec_reduce_max_ind (array2[:]); /* this should be OK. */

  x = __sec_reduce_min_ind (array[:][:]); /* { dg-error "cannot have arrays with dimension greater than" } */

  y = __sec_reduce_min_ind (array2[:]); /* this should be OK. */

  return 0;
}
