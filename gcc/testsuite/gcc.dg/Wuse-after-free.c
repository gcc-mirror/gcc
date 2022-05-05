/* PR middle-end/104069 - -Werror=use-after-free false positive on
   elfutils-0.186
   { dg-do compile }
   { dg-options "-Wall" } */

typedef __SIZE_TYPE__ size_t;

extern void* realloc (void *, size_t);

void* __libdw_unzstd (size_t todo)
{
  void *sb = 0;

  for ( ; ; )
    {
      // Ran only once.
      if (!sb)
	{
	  char *b = realloc (sb, todo);
	  if (!b)
	    break;

	  sb = b;
	}

      todo -= 1;
      if (todo == 0)
	break;
    }

  // Shrink buffer: leave only one byte for simplicity.
  char *b = realloc (sb, 1);
  if (b)
    sb = b;
  else
    {
      // Realloc failed mysteriously, leave 'sb' untouched.
    }

  return sb;        // { dg-bogus "-Wuse-after-free" }
}
