int __attribute__((ms_abi))
va_demo (int count, ...)
{
  int sum = 0;
  __builtin_ms_va_list ap;

  __builtin_ms_va_start (ap, count);
  while (count)
    {
      sum += __builtin_va_arg (ap, int);
      --count;
    }

  __builtin_ms_va_end (ap);
  return sum;
}
