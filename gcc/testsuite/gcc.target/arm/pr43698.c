/* { dg-do run } */
/* { dg-options "-Os" } */
#include <stdint.h>
#include <stdlib.h>


char do_reverse_endian = 0;

#  define bswap_32(x) \
  ((((x) & 0xff000000) >> 24) | \
   (((x) & 0x00ff0000) >>  8) | \
   (((x) & 0x0000ff00) <<  8) | \
   (((x) & 0x000000ff) << 24))

#define EGET(X) \
  (__extension__ ({ \
      uint64_t __res; \
      if (!do_reverse_endian) {    __res = (X); \
      } else if (sizeof(X) == 4) { __res = bswap_32((X)); \
      } \
      __res; \
    }))

void __attribute__((noinline)) X(char **phdr, char **data, int *phoff)
{
  *phdr = *data + EGET(*phoff);
}

int main()
{
  char *phdr;
  char *data = (char *)0x40164000;
  int phoff = 0x34;
  X(&phdr, &data, &phoff);
  if (phdr != (char *)0x40164034)
    abort ();
  exit (0);
}
