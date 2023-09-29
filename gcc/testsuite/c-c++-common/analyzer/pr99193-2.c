/* Verify absence of false positive from -Wanalyzer-mismatching-deallocation
   on realloc(3).
   Based on https://github.com/libguestfs/libguestfs/blob/f19fd566f6387ce7e4d82409528c9dde374d25e0/df/main.c#L404
   which is GPLv2 or later.  */

/* { dg-additional-options "-Wno-analyzer-too-complex" } */

typedef __SIZE_TYPE__ size_t;
typedef __builtin_va_list va_list;

#include "../../gcc.dg/analyzer/analyzer-decls.h"

extern void free (void *);
extern void *realloc (void *__ptr, size_t __size)
  __attribute__ ((__nothrow__ , __leaf__))
  __attribute__ ((__warn_unused_result__))
  __attribute__ ((__alloc_size__ (2)));
char *strdup (const char *)
  __attribute__((malloc (free)));

extern void error (int __status, int __errnum, const char *__format, ...)
     __attribute__ ((__format__ (__printf__, 3, 4)));

extern int errno;

struct drv
{
  struct drv *next;
};

#define EXIT_FAILURE 1

static char *
single_drive_display_name (struct drv *)
{
  char *result = strdup ("placeholder");
  if (!result)
    __builtin_abort ();
  return result;
}

char *
make_display_name (struct drv *drvs)
{
  char *ret;

  if (drvs->next == NULL)
    ret = single_drive_display_name (drvs);
  else {
    size_t pluses = 0;
    size_t i, len;

    while (drvs->next != NULL) {
      drvs = drvs->next;
      pluses++;
    }

    ret = single_drive_display_name (drvs);
    len = __builtin_strlen (ret);

    ret = (char *) realloc (ret, len + pluses + 1); /* { dg-bogus "'free'" } */
    if (ret == NULL)
      error (EXIT_FAILURE, errno, "realloc");
    for (i = len; i < len + pluses; ++i)
      ret[i] = '+';
    ret[i] = '\0';
  }

  return ret;
}
