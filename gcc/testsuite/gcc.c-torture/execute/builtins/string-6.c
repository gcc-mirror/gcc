/* Copyright (C) 2001  Free Software Foundation.

   Ensure that builtin memcmp operations when all three arguments
   are constant is optimized and performs correctly.  Taken from
   PR optimize/3508.

   Written by Roger Sayle, 12/26/2001.  */

extern void abort (void);
extern void link_error (void);

typedef __SIZE_TYPE__ size_t;
extern int memcmp (const void *, const void *, size_t);

void
main_test (void)
{
  if (memcmp ("abcd", "efgh", 4) >= 0)
    link_error ();
  if (memcmp ("abcd", "abcd", 4) != 0)
    link_error ();
  if (memcmp ("efgh", "abcd", 4) <= 0)
    link_error ();
}

#ifndef __OPTIMIZE__
/* When not optimizing, the above tests may generate references to
   the function link_error, but should never actually call it.  */
void
link_error ()
{
  abort ();
}
#endif
