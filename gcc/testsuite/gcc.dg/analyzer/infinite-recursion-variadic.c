int test_variadic_1 (int n, ...)
{
  __builtin_va_list args;
  int total =0;
  int i;

  __builtin_va_start(args, n);

  for (i = 0; i < n; i++)
    total += __builtin_va_arg(args, int);

  __builtin_va_end(args);

  return total;  
}

int test_variadic_2 (int n, ...)
{
  return test_variadic_2 (n, 42); /* { dg-warning "-Wanalyzer-infinite-recursion" } */
}

int test_variadic_3 (int n, ...)
{
  if (n > 0) /* { dg-message "when 'n > 0'" } */
    return test_variadic_3 (n, 42); /* { dg-warning "-Wanalyzer-infinite-recursion" } */
  return 0;
}

int test_variadic_4 (int n, ...)
{
  if (n > 0)
    return test_variadic_4 (n - 1, 42);
  return 0;
}
