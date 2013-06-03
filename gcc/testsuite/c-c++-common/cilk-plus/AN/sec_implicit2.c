/* { dg-do compile } */
/* { dg-options "-fcilkplus" } */

int main2 (int argc, char **argv);
int main (int argc, char **argv)
{
  int x = 0;
  if (argc == 1)
    {
      const char *array[] = {"a.out", "5"};
      x = main2 (2, (char**)array);
    }
  else
    x = main2 (argc, argv);

  return x;
}

int main2 (int argc, char **argv)
{
  int array[10][10], array2[10];

  array[:][:] = __sec_implicit_index(argc) + array[:][:]; /* { dg-error "__sec_implicit_index parameter" } */
  return 0;
}
