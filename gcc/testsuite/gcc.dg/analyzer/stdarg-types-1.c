/* { dg-require-effective-target int32 } */
/* { dg-require-effective-target lp64 } */

/* Type mismatch: expect long, but passed an int.  */

static void __attribute__((noinline))
__analyzer_consume_long (int placeholder, ...)
{
  long v;
  __builtin_va_list ap;
  __builtin_va_start (ap, placeholder);
  v = __builtin_va_arg (ap, long); /* { dg-warning "'va_arg' expected 'long int' but received 'int' for variadic argument 1 of 'ap'" } */
  __builtin_va_end (ap);
}

void test_int_to_long (void)
{
  __analyzer_consume_long (42, 1066);
}

void test_char_to_long (void)
{
  /* char promoted to int.  */
  __analyzer_consume_long (42, 'a');
}
