/* Verify absence of false positive from -Wanalyzer-mismatching-deallocation
   on realloc(3).
   Based on https://github.com/libguestfs/libguestfs/blob/f19fd566f6387ce7e4d82409528c9dde374d25e0/daemon/debug.c#L115
   which is GPLv2 or later.  */

typedef __SIZE_TYPE__ size_t;
typedef __builtin_va_list va_list;

#define NULL ((void *)0)

extern void free (void *);
extern void *realloc (void *__ptr, size_t __size)
  __attribute__ ((__nothrow__ , __leaf__))
  __attribute__ ((__warn_unused_result__))
  __attribute__ ((__alloc_size__ (2)));
extern char *strdup (const char *)
  __attribute__((malloc (free)));
extern char *strcat (char *__restrict __dest, const char *__restrict __src)
  __attribute__ ((__nothrow__ , __leaf__))
  __attribute__ ((__nonnull__ (1, 2)));

static char *
debug_help (const char **cmds, size_t argc, char *const *const argv)
{
  size_t len, i;
  char *r, *p;

  r = strdup ("Commands supported:");
  if (!r) {
    return NULL;
  }

  len = __builtin_strlen (r);
  for (i = 0; cmds[i] != NULL; ++i) {
    len += __builtin_strlen (cmds[i]) + 1;
    p = realloc (r, len + 1); /* { dg-bogus "'free'" } */
    if (p == NULL) {
      free (r);
      return NULL;
    }
    r = p;

    strcat (r, " ");
    strcat (r, cmds[i]);
  }

  return r;
}
