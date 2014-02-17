/* Test for cross x86_64<->w64 abi standard calls via variable.  */
/* { dg-options "-O2 -mabi=ms -std=gnu99 -ffast-math -fno-builtin" } */

typedef int (*func)(void *, char *, char *, short, long long);

static int
callback (void *ptr, char *string1, char *string2, short number,
	  long long rand)
{
  if (ptr != 0
      || string1 != 0
      || string2 != 0
      || number != 0x1234
      || rand != 0x1234567890abcdefLL)
    return 1;
  else
    return 0;
}

func
get_callback (void)
{
  return callback;
}
