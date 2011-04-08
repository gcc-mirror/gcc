/* { dg-do assemble } */
/* { dg-options "-mthumb -Os -save-temps" }  */
/* { dg-require-effective-target arm_thumb2_ok } */

#include <stdio.h>

int getFileStartAndLength (int fd, int *start_, size_t *length_)
{
      int start, end;
      size_t length;

      start = lseek (fd, 0L, SEEK_CUR);
      end = lseek (fd, 0L, SEEK_END);

      if (start == -1 || end == -1)
         return -1;

      length = end - start;
      if (length == 0)
         return -1;

      *start_ = start;
      *length_ = length;

      return 0;
}

/* { dg-final { scan-assembler-times "pop" 2 } } */
/* { dg-final { scan-assembler-times "beq" 3 } } */
/* { dg-final { object-size text <= 54 } } */
