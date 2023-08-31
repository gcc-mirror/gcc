/* PR target/110411 */
/* { dg-options "-O2 -mdejagnu-cpu=power10 -mblock-ops-vector-pair" } */

/* Verify we do not ICE on the following.  */

#include <string.h>

struct s {
  long a;
  long b;
  long c;
  long d: 1;
};
unsigned long ptr;

void
bug (struct s *dst)
{
  struct s *src = (struct s *)(ptr & ~0xFUL);
  memcpy (dst, src, sizeof(struct s));
}
