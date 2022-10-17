/* Should be OK to add a "const" qualifier to a ptr.  */

static void __attribute__((noinline))
__analyzer_consume_const_char_ptr (int placeholder, ...)
{
  const char *v;
  __builtin_va_list ap;
  __builtin_va_start (ap, placeholder);
  v = __builtin_va_arg (ap, const char *);
  __builtin_va_end (ap);
}

void test_char_ptr_to_const_char_ptr (char *p)
{
  __analyzer_consume_const_char_ptr (42, p); /* { dg-bogus "" } */
}

/* What about casting away const-ness?
   Currently we don't complain.  */

static void __attribute__((noinline))
__analyzer_consume_char_ptr (int placeholder, ...)
{
  char *v;
  __builtin_va_list ap;
  __builtin_va_start (ap, placeholder);
  v = __builtin_va_arg (ap, char *);
  __builtin_va_end (ap);
}

void test_const_char_ptr_to_char_ptr (const char *p)
{
  __analyzer_consume_const_char_ptr (42, p);
}

/* What about casting ptrs?
   Currently we don't complain.  */

struct foo;
struct bar;

static void __attribute__((noinline))
__analyzer_consume_bar_ptr (int placeholder, ...)
{
  struct bar *v;
  __builtin_va_list ap;
  __builtin_va_start (ap, placeholder);
  v = __builtin_va_arg (ap, struct bar *);
  __builtin_va_end (ap);
}

void test_foo_ptr_to_bar_ptr (struct foo *p)
{
  __analyzer_consume_bar_ptr (42, p);
}
