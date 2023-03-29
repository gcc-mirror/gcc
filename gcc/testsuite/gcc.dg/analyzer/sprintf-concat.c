typedef __SIZE_TYPE__ size_t;
#define NULL ((void *)0)

extern size_t
strlen(const char* __s) __attribute__((__nothrow__, __leaf__))
__attribute__((__pure__)) __attribute__((__nonnull__(1)));

extern void*
malloc(size_t __size) __attribute__((__nothrow__, __leaf__))
__attribute__((__malloc__)) __attribute__((__alloc_size__(1)));

extern int
sprintf(char* __restrict __s, const char* __restrict, ...)
  __attribute__((__nothrow__));

char *
test_1 (const char *a, const char *b)
{
  size_t sz = strlen (a) + strlen (b) + 2;
  char *p = malloc (sz);
  if (!p)
    return NULL;
  sprintf (p, "%s/%s", a, b);
  return p;
}

void
test_2 (const char *a, const char *b)
{
  size_t sz = strlen (a) + strlen (b) + 2;
  char *p = malloc (sz); /* { dg-message "allocated here" } */
  if (!p)
    return;
  sprintf (p, "%s/%s", a, b); /* { dg-warning "leak of 'p' " } */
}
