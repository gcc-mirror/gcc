/* PR sanitizer/58413 */
/* { dg-do run { target int32plus } } */
/* { dg-options "-fsanitize=shift -w" } */

#include <stdio.h>

int x = 7;
int
main (void)
{
  fputs ("UBSAN TEST START\n", stderr);

  /* All of the following should pass.  */
  int A[128 >> 5] = {};
  int B[128 << 5] = {};

  static int e =
    ((int)
     (0x00000000 | ((31 & ((1 << (4)) - 1)) << (((15) + 6) + 4)) |
      ((0) << ((15) + 6)) | ((0) << (15))));

  if (e != 503316480)
    __builtin_abort ();

  switch (x)
    {
    case 1 >> 4:
    case 1 << 4:
    case 128 << (4 + 1):
    case 128 >> (4 + 1):
      return 1;
    }

  fputs ("UBSAN TEST END\n", stderr);
  return 0;
}

/* { dg-output "UBSAN TEST START(\n|\r\n|\r)UBSAN TEST END" } */
