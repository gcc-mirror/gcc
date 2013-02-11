/* { dg-do run { target powerpc*-*-eabi* powerpc*-*-elf* powerpc*-*-linux* powerpc*-*-rtems* } } */
/* { dg-options "-O -mlong-double-128" } */

#include <stdlib.h>

/* SVR4 and EABI both specify that 'long double' is aligned to a 128-bit
   boundary in structures.  */

struct {
  int x;
  long double d;
} s;

int main(void)
{
  if (sizeof (s) != 32)
    abort ();
  if ((char *)&s.d - (char *)&s != 16)
    abort ();
  exit (0);
}
