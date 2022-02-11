/* PR tree-optimization/103456 */
/* { dg-do compile } */
/* { dg-options "-fsanitize=undefined -O -fdump-tree-objsz -ffat-lto-objects" } */

static char *multilib_options = "m64/m32";

void
used_arg_t (void)
{
  char *q = multilib_options;
  for (;;)
    {
      while (*q)
	q++;
      while (__builtin_strchr (q, '_') == 0)
	while (*q)
	  q++;
    }
}

/* { dg-final { scan-tree-dump-not "maximum object size 0" "objsz1" } } */
