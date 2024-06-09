extern void unknown_returns (const char *p);
extern void unknown_noreturn (const char *p) __attribute__((__noreturn__));

void test_1 (const char *p)
{
  if (p)
    unknown_returns (p);
  __builtin_strcmp ("a", p); /* { dg-message "pointer 'p' is dereferenced here" "" { target c } } */
  if (p) /* { dg-warning "check of 'p' for NULL after already dereferencing it" "" { target c } } */
    unknown_returns (p);
  __builtin_strcmp ("a", p);  
}

void test_2 (const char *p)
{
  if (p)
    unknown_noreturn (p);
  __builtin_strcmp ("a", p);
  if (p) /* { dg-bogus "check of 'p' for NULL after already dereferencing it" } */
    unknown_noreturn (p);
  __builtin_strcmp ("a", p);  
}
