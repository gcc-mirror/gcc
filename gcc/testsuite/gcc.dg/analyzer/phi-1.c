/* { dg-do "compile" } */

typedef __SIZE_TYPE__ size_t;
#define NULL ((void *) 0)

extern const char *foo (void);
extern size_t bar (void);

void
_nl_expand_alias (const char *locale_alias_path)
{
  size_t added;
  do
    {
      added = 0;
      while (added == 0 && locale_alias_path[0] != '\0')
	{
	  const char *start = foo ();
	  if (start < locale_alias_path)
	    added = bar ();
	}
    }
  while (added != 0);
}
