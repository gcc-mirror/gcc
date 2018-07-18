// { dg-do run }

#define FN(NAME) \
NAME (void) \
{ \
  char *ptr; \
  char *ptr2; \
  { \
    char my_char[9]; \
    ptr = &my_char[0]; \
    __builtin_memcpy (&ptr2, &ptr, sizeof (ptr2)); \
  } \
 \
  *(ptr2+9) = 'c'; \
}

void
__attribute__((no_sanitize(("address"))))
__attribute__((no_sanitize(("undefined"))))
__attribute__((no_sanitize(("address"))))
__attribute__((no_sanitize(("null"))))
FN (fn1)

void
__attribute__((no_sanitize(("all"))))
FN (fn2)

void
__attribute__((no_sanitize_address))
FN (fn3)

int
main (void)
{
  fn1 ();
  fn2 ();
  fn3 ();

  return 0;
}
