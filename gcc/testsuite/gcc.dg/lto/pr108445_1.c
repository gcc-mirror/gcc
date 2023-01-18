extern int gArray[];

int foo(int *a)
{
  int *p = a;

  return *p;
}

int main(int argc, char *argv[])
{
  if (argc & 1)
    gArray[argc - 1] = 1;

  if (argc > 1)
    return foo(gArray);

  return 0;
}
