/* { dg-do run } */

typedef unsigned short	INT16U;

typedef struct tst_2
{
  INT16U	f0;	// [+0]
  INT16U *	f1;	// [+2]
  INT16U	f2;	// [+6]
  INT16U *	f3;	// [+8]
} __attribute__ ((__packed__)) t2;

#include <stddef.h>
#include <stdlib.h>

int main (void)
{
  if (offsetof (t2, f1) != 2)
    abort ();
  if (offsetof (t2, f2) != 6)
    abort ();
  if (offsetof (t2, f3) != 8)
    abort ();
  exit (0);
}
