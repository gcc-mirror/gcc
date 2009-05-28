/* Failed on powerpc64-linux with a segfault due to ifcvt generating
   conditional returns without updating dominance info.
   Extracted from glibc's dl-load.c.  */

typedef __SIZE_TYPE__ size_t;

static size_t
is_dst (const char *start, const char *name, const char *str,
        int is_path, int secure)
{
  size_t len;
  _Bool is_curly = 0;

  if (name[0] == '{')
    {
      is_curly = 1;
      ++name;
    }

  len = 0;
  while (name[len] == str[len] && name[len] != '\0')
    ++len;

  if (is_curly)
    {
      if (name[len] != '}')
        return 0;


      --name;

      len += 2;
    }
  else if (name[len] != '\0' && name[len] != '/'
           && (!is_path || name[len] != ':'))
    return 0;

  if (__builtin_expect (secure, 0)
      && ((name[len] != '\0' && (!is_path || name[len] != ':'))
          || (name != start + 1 && (!is_path || name[-2] != ':'))))
    return 0;

  return len;
}
