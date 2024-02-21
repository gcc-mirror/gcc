/* Verify we don't ICE on -Wanalyzer-overlapping-buffers on
   execution paths where the size is constant zero, but the
   optimizer didn't see that.  */

typedef __SIZE_TYPE__ size_t;

extern char a[];
size_t n;

size_t  __attribute__((noinline))
get_hidden_zero ()
{
  return 0;
}

void
test_pr113998 ()
{
  size_t n = get_hidden_zero ();
  __builtin_strncpy (a, a, n); /* { dg-warning "overlapping buffers passed as arguments to" } */
}
