/* Regression test for PR analyzer/114920 (missing warnings about
   unterminated strings passed to strlen).  */

#define NULL ((void *)0)

void
test_stack ()
{
  char s[7];
  __builtin_memcpy (s, "FOOBAR!", 7);
  __builtin_printf ("n=%d\n", (int) __builtin_strlen (s)); /* { dg-warning "stack-based buffer over-read" "PR analyzer/114920 on stack" } */
  /* { dg-message "while looking for null terminator" "note" { target *-*-* } .-1 } */ 
}

char *
test_heap ()
{
  char *s = (char *)__builtin_malloc (7);
  if (s != NULL) {
    __builtin_memcpy (s, "FOOBAR!", 7);
    __builtin_printf ("n=%d\n", (int) __builtin_strlen (s)); /* { dg-warning "heap-based buffer over-read" "PR analyzer/114920 on heap" } */
    /* { dg-message "while looking for null terminator" "note" { target *-*-* } .-1 } */ 
  }
  return s;  
}
