/* PR tree-optimization/97081 */

#include "tree-vect.h"

unsigned short s[1024];
unsigned char c[1024];

__attribute__((noipa)) void
foo (int n)
{
  for (int i = 0; i < 1024; i++)
    s[i] = (s[i] << n) | (s[i] >> (__SIZEOF_SHORT__ * __CHAR_BIT__ - n));
  for (int i = 0; i < 1024; i++)
    c[i] = (c[i] << n) | (c[i] >> (__CHAR_BIT__ - n));
}

int
main ()
{
  check_vect ();
  for (int i = 0; i < 1024; i++)
    {
      s[i] = i;
      c[i] = i;
    }
  foo (3);
  for (int i = 0; i < 1024; i++)
    if (s[i] != (unsigned short) ((i << 3) | (i >> (__SIZEOF_SHORT__ * __CHAR_BIT__ - 3)))
        || c[i] != (unsigned char) ((((unsigned char) i) << 3) | (((unsigned char) i) >> (__CHAR_BIT__ - 3))))
      __builtin_abort ();
  return 0;
}
