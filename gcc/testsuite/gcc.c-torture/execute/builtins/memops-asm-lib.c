extern void abort (void);
extern int inside_main;
typedef __SIZE_TYPE__ size_t;

#define TEST_ABORT if (inside_main) abort()

/* LTO code is at the present to able to track that asm alias my_bcopy on builtin
   actually refers to this function.  See PR47181. */
__attribute__ ((used))
void *
my_memcpy (void *d, const void *s, size_t n)
{
  char *dst = (char *) d;
  const char *src = (const char *) s;
  while (n--)
    *dst++ = *src++;
  return (char *) d;
}

/* LTO code is at the present to able to track that asm alias my_bcopy on builtin
   actually refers to this function.  See PR47181. */
__attribute__ ((used))
void
my_bcopy (const void *s, void *d, size_t n)
{
  char *dst = (char *) d;
  const char *src = (const char *) s;
  if (src >= dst)
    while (n--)
      *dst++ = *src++;
  else
    {
      dst += n;
      src += n;
      while (n--)
        *--dst = *--src;
    }
}

__attribute__ ((used))
void *
my_memmove (void *d, const void *s, size_t n)
{
  char *dst = (char *) d;
  const char *src = (const char *) s;
  if (src >= dst)
    while (n--)
      *dst++ = *src++;
  else
    {
      dst += n;
      src += n;
      while (n--)
	*--dst = *--src;
    }

  return d;
}

/* LTO code is at the present to able to track that asm alias my_bcopy on builtin
   actually refers to this function.  See PR47181. */
__attribute__ ((used))
void *
my_memset (void *d, int c, size_t n)
{
  char *dst = (char *) d;
  while (n--)
    *dst++ = c;
  return (char *) d;
}

/* LTO code is at the present to able to track that asm alias my_bcopy on builtin
   actually refers to this function.  See PR47181. */
__attribute__ ((used))
void
my_bzero (void *d, size_t n)
{
  char *dst = (char *) d;
  while (n--)
    *dst++ = '\0';
}

void *
memcpy (void *d, const void *s, size_t n)
{
  void *result = my_memcpy (d, s, n);
  TEST_ABORT;
  return result;
}

void
bcopy (const void *s, void *d, size_t n)
{
  my_bcopy (s, d, n);
  TEST_ABORT;
}

void *
memset (void *d, int c, size_t n)
{
  void *result = my_memset (d, c, n);
  TEST_ABORT;
  return result;
}

void
bzero (void *d, size_t n)
{
  my_bzero (d, n);
  TEST_ABORT;
}

#ifdef __vxworks
/* The RTP C library uses bfill, which is defined in the same file as
   bzero and bcopy.  */
#include "lib/bfill.c"
#endif
