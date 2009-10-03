#include "20081222_0.h"

/* Actually, call "x" "INT_X", and make it hidden.  */
extern __typeof (x) x
	__asm__ ("INT_x")
	__attribute__ ((__visibility__ ("hidden")));

int x ()
{
  return 7;
}

/* Make an externally-visible symbol "X" that's an alias for INT_x.  */
extern __typeof (x) EXT_x
	__asm__ ("x")
	__attribute__ ((__alias__ ("INT_x")));
