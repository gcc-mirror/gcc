/* Test whether store motion recognizes pure functions as potentially reading
   any memory.  */

typedef __SIZE_TYPE__ size_t;
extern void *memcpy (void *dest, const void *src, size_t n);
extern size_t strlen (const char *s);
extern int strcmp (const char *s1, const char *s2) __attribute__((pure));

char buf[50];

static void foo (void)
{
  if (memcpy (buf, "abc", 4) != buf) abort ();
  if (strcmp (buf, "abc")) abort ();
  memcpy (buf, "abcdefgh", strlen ("abcdefgh") + 1);
}

int main (void)
{
  foo ();
  return 0;
}
