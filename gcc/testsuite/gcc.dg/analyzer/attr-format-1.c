extern int
my_printf (void *my_object, const char *my_format, ...)
  __attribute__ ((format (printf, 2, 3)));
/* { dg-message "parameter 2 of 'my_printf' marked as a format string via 'format' attribute" "attr note" { target *-*-* } .-2 } */
/* { dg-message "argument 2 of 'my_printf' must be a pointer to a null-terminated string" "arg note" { target *-*-* } .-3 } */

int test_empty (void *my_object, const char *msg)
{
  return my_printf (my_object, "");
}

int test_percent_s (void *my_object, const char *msg)
{
  return my_printf (my_object, "%s\n", msg);
}

int
test_unterminated_format (void *my_object)
{
  char fmt[3] = "abc";
  return my_printf (my_object, fmt); /* { dg-warning "stack-based buffer over-read" } */
  /* { dg-message "while looking for null terminator for argument 2 \\('&fmt'\\) of 'my_printf'..." "event" { target *-*-* } .-1 } */
}

int
test_uninitialized_format (void *my_object)
{
  char fmt[10];
  return my_printf (my_object, fmt); /* { dg-warning "use of uninitialized value 'fmt\\\[0\\\]'" } */  
  /* { dg-message "while looking for null terminator for argument 2 \\('&fmt'\\) of 'my_printf'..." "event" { target *-*-* } .-1 } */
}
