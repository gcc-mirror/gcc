/* Copyright (C) 2001  Free Software Foundation.

   Ensure all builtin strlen comparisons against zero are optimized
   and perform correctly. The multiple calls to strcpy are to prevent
   the potentially "pure" strlen calls from being removed by CSE.

   Written by Roger Sayle, 11/02/2001.  */

extern void abort (void);
typedef __SIZE_TYPE__ size_t;
extern size_t strlen (const char *);
extern char *strcpy (char *, const char *);

int
main ()
{
  char str[8];
  char *ptr;

  ptr = str;
  strcpy (ptr, "nts");
  if (strlen (ptr) == 0)
    abort ();

  strcpy (ptr, "nts");
  if (strlen (ptr) < 1)
    abort ();

  strcpy (ptr, "nts");
  if (strlen (ptr) <= 0)
    abort ();

  strcpy (ptr, "nts");
  if (strlen (ptr+3) != 0)
    abort ();

  strcpy (ptr, "nts");
  if (strlen (ptr+3) > 0)
    abort ();

  strcpy (ptr, "nts");
  if (strlen (str+3) >= 1)
    abort ();

  return 0;
}

#ifdef __OPTIMIZE__
/* When optimizing, all the above cases should be transformed into
   something else.  So any remaining calls to the original function
   should abort.  */
static size_t
strlen (const char *s)
{
  abort ();
}
#endif

