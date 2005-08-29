/* PR middle-end/23484 */

extern void abort (void);
typedef __SIZE_TYPE__ size_t;
extern size_t strlen (const char *);
extern void *memcpy (void *, const void *, size_t);
extern void *mempcpy (void *, const void *, size_t);
extern void *memmove (void *, const void *, size_t);
extern int snprintf (char *, size_t, const char *, ...);
extern int memcmp (const void *, const void *, size_t);

#include "chk.h"

static char data[8] = "ABCDEFG";

int l1;

void
__attribute__((noinline))
test1 (void)
{
  char buf[8];

  /* All the checking calls in this routine have a maximum length, so
     object size checking should be done at compile time if optimizing.  */
  chk_calls = 0;

  memset (buf, 'I', sizeof (buf));
  if (memcpy (buf, data, l1 ? sizeof (buf) : 4) != buf
      || memcmp (buf, "ABCDIIII", 8))
    abort ();

  memset (buf, 'J', sizeof (buf));
  if (mempcpy (buf, data, l1 ? sizeof (buf) : 4) != buf + 4
      || memcmp (buf, "ABCDJJJJ", 8))
    abort ();

  memset (buf, 'K', sizeof (buf));
  if (memmove (buf, data, l1 ? sizeof (buf) : 4) != buf
      || memcmp (buf, "ABCDKKKK", 8))
    abort ();

  memset (buf, 'L', sizeof (buf));
  if (snprintf (buf, l1 ? sizeof (buf) : 4, "%d", l1 + 65536) != 5
      || memcmp (buf, "655\0LLLL", 8))
    abort ();

  if (chk_calls)
    abort ();
}

void
main_test (void)
{
#ifndef __OPTIMIZE__
  /* Object size checking is only intended for -O[s123].  */
  return;
#endif
  __asm ("" : "=r" (l1) : "0" (l1));
  test1 ();
}
