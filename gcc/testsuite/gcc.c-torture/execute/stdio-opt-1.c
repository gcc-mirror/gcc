/* Copyright (C) 2000  Free Software Foundation.

   When eliminating NOP calls to builtin fputs, ensure that we still
   evaluate the stream argument in case it has side effects.
   Written by Kaveh R. Ghazi, 10/30/2000.  */

#include <stdio.h>

int main()
{
  FILE *s_array[3] = {stdout, NULL, stdout}, **s_ptr = s_array;
  
  /* Increment the stream pointer once.  */
  fputs ("", *s_ptr++);

  /* Increment the stream pointer a second time.  */
  s_ptr++;

  /* If we failed to increment the stream pointer twice, then the
     stream passed in here will be NULL and we should crash.  */
  fputs ("hello world\n", *s_ptr);
  
  /* Just in case, If *s_ptr is NULL abort anyway.  */
  if (*s_ptr == 0)
    abort();

  return 0;
}
