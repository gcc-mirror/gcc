#include <stdlib.h>
#include <string.h>

#define nul  (char)0
struct m2string {
  char *contents;
  int HIGH;
};

typedef struct m2string STRING;
static inline int inline StrLen (STRING a) __attribute__ ((always_inline));

static STRING b;

void init (void)
{
  b.contents = "hello";
  b.HIGH = 4;

  if (StrLen(b) == 5)
    write(1, "works\n", 6);
}

static inline int StrLen (STRING a)
{
  int **T20;
  char *T24;
#if 0
  char *copy;  
  int high, len;

  copy = alloca(a.HIGH+1);
  memcpy(a.contents, copy, a.HIGH+1);
  a.contents = copy;

  len = 0;
  high = a.HIGH;
  T25 = (char **)&a;
  **T25 = 'a';
#endif

#if 0
  /* (a.contents[len] != nul) */
  while ((len <= high) && ((*T24)[len] != nul))
    len++;

  return len;
#endif
  (*(char **)&a)[0] = 'a';

  T20 = &a;
  T24 = *T20;
  *T24 = 'a';
  return 5;
}

