#pragma GCC system_header
typedef __SIZE_TYPE__ size_t;
extern void *memset (void *s, int c, size_t n)
  __attribute__ ((nothrow, nonnull (1)));
extern inline
__attribute__ ((always_inline, artificial, gnu_inline, nothrow))
void *
memset (void *dest, int ch, size_t len)
{
  return __builtin___memset_chk (dest, ch, len,
				 __builtin_object_size (dest, 0));
}

#ifdef TEST2
static void
__attribute__ ((noinline))
test2 (void)
{
  char buf[4];
  memset (buf, 0, 6);
}
#endif

#ifdef TEST3
static inline void
__attribute__ ((always_inline))
test3 (char *p)
{
  memset (p, 0, 6);
}
#endif
