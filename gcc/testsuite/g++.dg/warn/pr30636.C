/* { dg-do compile } */
/* { dg-options "-O2 -Warray-bounds" } */

typedef char one_buffer[512];
static one_buffer emergency_buffer[4];

void free_exception (void *vptr)
{
  char *base = (char *) &emergency_buffer[0][0];
  char *ptr = (char *) vptr;
  if (ptr >= base && ptr < base + sizeof (emergency_buffer)) /* { dg-bogus "subscript" } */
    {
      /* Do something. */
      __builtin_exit (0);
    }
}

