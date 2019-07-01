/* { dg-lto-do run } */
/* { dg-lto-options { { -O2 -flto } } } */

/* With LTO we consider all pointers to incomplete types to be possibly
   aliasing.  This makes *bptr to alias with aptr.
   For C++ ODR types we however can work out that they are actually
   different.  */

#include <string.h>

typedef int (*fnptr) ();

__attribute__ ((used))
struct a *aptr;

__attribute__ ((used))
struct b **bptr = (struct b**)&aptr;
extern void init ();
extern void inline_me_late (int);


int
main (int argc, char **argv)
{
  init ();
  aptr = 0;
  inline_me_late (argc);
  if (!__builtin_constant_p (aptr == 0))
    __builtin_abort ();
  return (size_t)aptr;
}
