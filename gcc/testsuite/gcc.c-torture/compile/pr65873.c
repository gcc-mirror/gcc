typedef __SIZE_TYPE__ size_t;

extern inline __attribute__ ((__always_inline__, __gnu_inline__, __artificial__, __nothrow__, __leaf__)) void *
memcpy (void *__restrict __dest, const void *__restrict __src, size_t __len)
{
  return __builtin___memcpy_chk (__dest, __src, __len, __builtin_object_size (__dest, 0));
}

__attribute__((optimize ("Ofast"))) void
bar (void *d, void *s, size_t l)
{
  memcpy (d, s, l);
}

