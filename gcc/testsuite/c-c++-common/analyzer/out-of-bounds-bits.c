/* Coverage to exercise the various describe_final_event_as_bits
   functions in gcc/analyzer/bounds-checking.cc.  */

struct s1
{
  unsigned int a : 3;
  unsigned int b : 5;
  unsigned int c : 9;
};

void
test_concrete_buffer_overflow_bits (void)
{
  struct s1 *p = (struct s1 *)__builtin_malloc (sizeof (struct s1));
  if (!p)
    return;

  p[1].c = 1; /* { dg-warning "heap-based buffer overflow" } */
  /* { dg-message "out-of-bounds write from bit \[0-9\]* till bit \[0-9\]* but region ends at bit \[0-9\]*" "" { target *-*-* } .-1 } */

  __builtin_free (p);
}

int
test_concrete_buffer_over_read_bits (void)
{
  struct s1 *p = (struct s1 *)__builtin_malloc (sizeof (struct s1));
  if (!p)
    return -1;

  int v = p[1].c; /* { dg-warning "heap-based buffer over-read" } */
  /* { dg-message "out-of-bounds read from bit \[0-9\]* till bit \[0-9\]* but region ends at bit \[0-9\]*" "" { target *-*-* } .-1 } */

  __builtin_free (p);

  return v;
}

void
test_concrete_buffer_underwrite_bits (void)
{
  struct s1 *p = (struct s1 *)__builtin_malloc (sizeof (struct s1));
  if (!p)
    return;

  p[-1].a = 1; /* { dg-warning "heap-based buffer underwrite" } */
  /* { dg-message "out-of-bounds write from bit -\[0-9\]* till bit -\[0-9\]* but region starts at bit 0" "" { target *-*-* } .-1 } */

  __builtin_free (p);
}

int
test_concrete_buffer_under_read_bits (void)
{
  struct s1 *p = (struct s1 *)__builtin_malloc (sizeof (struct s1));
  if (!p)
    return -1;

  int v = p[-1].a; /* { dg-warning "heap-based buffer under-read" } */
  /* { dg-message "out-of-bounds read from bit -\[0-9\]* till bit -\[0-9\]* but region starts at bit 0" "" { target *-*-* } .-1 } */


  __builtin_free (p);

  return v;
}
