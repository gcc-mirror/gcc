/* { dg-additional-options "-fno-analyzer-call-summaries -fno-analyzer-state-merge -Wno-analyzer-too-complex" } */

void test_format_string (const char *fmt, ...)
{
  __builtin_va_list ap;
  __builtin_va_start (ap, fmt);
  while (*fmt)
    switch (*fmt++)
      {
      case 's':
	{
	  const char *s = __builtin_va_arg (ap, char *); /* { dg-warning "'va_arg' expected 'const char \\*' but received 'int' for variadic argument 1 of 'ap'" } */
	  __builtin_printf ("string: %s\n", s);
	}
	break;
      case 'd':
	{
	  int i = __builtin_va_arg (ap, int); /* { dg-warning "'va_arg' expected 'int' but received '\[^\n\r\]*' for variadic argument 1 of 'ap'" "type mismatch from wrong_type_for_percent_d" } */
	  /* { dg-warning "'ap' has no more arguments \\(1 consumed\\)" "not_enough_args" { target *-*-* } .-1 } */
	  __builtin_printf ("int: %d\n", i);
	}
	break;
      case 'c':
	{
	  char c = (char)__builtin_va_arg (ap, int);
	  __builtin_printf ("char: %c\n", c);
	}
	break;      
      }
  __builtin_va_end (ap);
}

void test_missing_va_start (const char *fmt, ...)
{
  __builtin_va_list ap;

  while (*fmt)
    switch (*fmt++)
      {
      case 's':
	{
	  const char *s = __builtin_va_arg (ap, char *); /* { dg-warning "use of uninitialized value 'ap'" } */
	  __builtin_printf ("string: %s\n", s);
	}
	break;
      case 'd':
	{
	  int i = __builtin_va_arg (ap, int); /* { dg-warning "use of uninitialized value 'ap'" } */
	  __builtin_printf ("int: %d\n", i); 
	}
	break;
      case 'c':
	{
	  char c = (char)__builtin_va_arg (ap, int); /* { dg-warning "use of uninitialized value 'ap'" } */
	  __builtin_printf ("char: %c\n", c);
	}
	break;      
      }
  __builtin_va_end (ap);
}

void test_missing_va_end (const char *fmt, ...)
{
  __builtin_va_list ap;
  __builtin_va_start (ap, fmt);
  while (*fmt)
    switch (*fmt++)
      {
      case 's':
	{
	  const char *s = __builtin_va_arg (ap, char *);
	  __builtin_printf ("string: %s\n", s);
	}
	break;
      case 'd':
	{
	  int i = __builtin_va_arg (ap, int);
	  __builtin_printf ("int: %d\n", i);
	}
	break;
      case 'c':
	{
	  char c = (char)__builtin_va_arg (ap, int);
	  __builtin_printf ("char: %c\n", c);
	}
	break;      
      }
} /* { dg-warning "missing call to 'va_end'" } */

void wrong_type_for_percent_s (void)
{
  test_format_string ("%s", 42);
}

void wrong_type_for_percent_d (void)
{
  test_format_string ("%d", "foo");
}

void not_enough_args (void)
{
  test_format_string ("%s%d", "foo");
}
