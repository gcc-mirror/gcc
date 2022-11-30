/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } { "" } } */

#include "../analyzer-decls.h"

extern _Bool quit_flag;
extern void char_charset (int);

static void
__analyzer_ccl_driver (int *source, int src_size)
{
  int *src = source, *src_end = src + src_size;
  int i = 0;

  while (!quit_flag)
    {
      if (src < src_end)
	{
	  __analyzer_dump_path (); /* { dg-message "path" } */
	  i = *src++; /* { dg-bogus "uninit" } */
	}
      char_charset (i);
    }
}

void
Fccl_execute_on_string (char *str, long str_bytes)
{
  while (1)
    {
      char *p = str;
      char *endp = str + str_bytes;
      int source[1024];
      int src_size = 0;

      while (src_size < 1024 && p < endp)
	{
	  __analyzer_dump_path (); /* { dg-message "path" } */
	  source[src_size++] = *p++;
	}

      __analyzer_ccl_driver (source, src_size);
    }
}
