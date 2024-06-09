typedef __SIZE_TYPE__ size_t;

extern size_t strlen (const char *__s)
  __attribute__ ((__nothrow__ , __leaf__))
  __attribute__ ((__pure__))
  __attribute__ ((__nonnull__ (1)));

extern char *read_file (const char *file);

size_t test_1 (const char *file)
{
  char *str = read_file (file);
  return strlen (str);
}
