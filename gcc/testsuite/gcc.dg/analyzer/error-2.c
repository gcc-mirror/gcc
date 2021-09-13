#define NULL ((void*)0)
typedef __SIZE_TYPE__ size_t;

extern int errno;

extern void free (void *);
char *strdup (const char *)
  __attribute__((malloc (free)));

extern size_t strlen (const char *__s)
  __attribute__ ((__nothrow__ , __leaf__))
  __attribute__ ((__pure__))
  __attribute__ ((__nonnull__ (1)));

extern void error (int __status, int __errnum, const char *__format, ...)
     __attribute__ ((__format__ (__printf__, 3, 4)));

extern void error_at_line (int __status, int __errnum, const char *__fname,
			   unsigned int __lineno, const char *__format, ...)
     __attribute__ ((__format__ (__printf__, 5, 6)));

/* PR analyzer/99196; extract taken from
     https://github.com/libguestfs/libguestfs/blob/f19fd566f6387ce7e4d82409528c9dde374d25e0/daemon/tar.c#L108
   (which is GPLv2 or later).  */

extern char *read_whole_file (const char *error_file, size_t *out);

#define EXIT_FAILURE 1

char *read_error_file (const char *error_file)
{
  size_t len;
  char *str;

  str = read_whole_file (error_file, &len);
  if (str == NULL) {
    str = strdup ("(no error)");
    if (str == NULL)
      error (EXIT_FAILURE, errno, "strdup");
    len = strlen (str); /* { dg-bogus "NULL" } */
  }

  /* Remove trailing \n character if any. */
  if (len > 0 && str[len-1] == '\n')
    str[--len] = '\0';

  return str;                   /* caller frees */
}
