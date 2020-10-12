/* Test case derived from Binutils/GDB's readline/readline/histexpand.c.
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

char *
get_subst_pattern (char *str, int *iptr, int delimiter, int is_rhs, int *lenptr)
{
  int si, i, j, k;
  char *s;

  s = 0;
  i = *iptr;

  for (si = i; str[si] && str[si] != delimiter; si++)
      if (str[si] == '\\' && str[si + 1] == delimiter)
	si++;

  if (si > i || is_rhs)
    {
      s = (char *)__builtin_malloc (si - i + 1);
      for (j = 0, k = i; k < si; j++, k++)
	{
	  /* Remove a backslash quoting the search string delimiter. */
	  if (str[k] == '\\' && str[k + 1] == delimiter)
	    k++;
	  s[j] = str[k];   // { dg-bogus "-Wstringop-overflow" }
	}
      s[j] = '\0';
      if (lenptr)
	*lenptr = j;
    }

  return s;
}
