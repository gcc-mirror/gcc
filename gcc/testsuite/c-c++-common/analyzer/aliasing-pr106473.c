void foo(char **args[], int *argc)
{
  *argc = 1;
  (*args)[0] = (char *) __builtin_malloc(42);
}
