/* Verify absence of false positive from -Wanalyzer-mismatching-deallocation
   on realloc(3).
   Based on https://github.com/libguestfs/libguestfs/blob/f19fd566f6387ce7e4d82409528c9dde374d25e0/daemon/command.c#L115
   which is GPLv2 or later.  */

typedef __SIZE_TYPE__ size_t;
typedef __builtin_va_list va_list;

#define NULL ((void *)0)

extern void *malloc (size_t __size)
  __attribute__ ((__nothrow__ , __leaf__))
  __attribute__ ((__malloc__))
  __attribute__ ((__alloc_size__ (1)));
extern void perror (const char *__s);
extern void *realloc (void *__ptr, size_t __size)
  __attribute__ ((__nothrow__ , __leaf__))
  __attribute__ ((__warn_unused_result__))
  __attribute__ ((__alloc_size__ (2)));

extern void guestfs_int_cleanup_free (void *ptr);
extern int commandrvf (char **stdoutput, char **stderror, unsigned flags,
                       char const* const *argv);
#define CLEANUP_FREE __attribute__((cleanup(guestfs_int_cleanup_free))) 

int
commandrf (char **stdoutput, char **stderror, unsigned flags,
           const char *name, ...)
{
  va_list args;
  CLEANUP_FREE const char **argv = NULL;
  char *s;
  int i, r;

  /* Collect the command line arguments into an array. */
  i = 2;
  argv = malloc (sizeof (char *) * i);

 if (argv == NULL) {
    perror ("malloc");
    return -1;
  }
  argv[0] = (char *) name;
  argv[1] = NULL;

  __builtin_va_start (args, name);

  while ((s = __builtin_va_arg (args, char *)) != NULL) {
    const char **p = realloc (argv, sizeof (char *) * (++i)); /* { dg-bogus "'free'" } */
    if (p == NULL) {
      perror ("realloc");
      __builtin_va_end (args);
      return -1;
    }
    argv = p;
    argv[i-2] = s;
    argv[i-1] = NULL;
  }

  __builtin_va_end (args);

  r = commandrvf (stdoutput, stderror, flags, argv);

  return r;
}
