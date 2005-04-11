/* { dg-do compile } */
/* { dg-options "-O2" } */

extern __SIZE_TYPE__ strlen (__const char *__s)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1)));
extern char *strcpy (char *__restrict __dest, __const char *__restrict __src)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));
extern char *getenv (__const char *__name) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));
extern int access (__const char *__name, int __type) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));
extern void * xmalloc (__SIZE_TYPE__) __attribute__ ((__malloc__));

static __inline__ const char *
try (const char *dir, const char *base)
{
  if (base != 0)
    return base;
  if (dir != 0
      && access (dir, 4 | 2 | 1) == 0)
    return dir;
  return 0;
}

static const char tmp[] = { '/', 't', 'm', 'p', 0 };
static const char usrtmp[] =
{ '/', 'u', 's', 'r', '/', 't', 'm', 'p', 0 };
static const char vartmp[] =
{ '/', 'v', 'a', 'r', '/', 't', 'm', 'p', 0 };

static char *memoized_tmpdir;
char *
choose_tmpdir (void)
{
  const char *base = 0;
  char *tmpdir;
  unsigned int len;

  if (memoized_tmpdir)
    return memoized_tmpdir;

  base = try (getenv ("TMPDIR"), base);
  base = try (getenv ("TMP"), base);
  base = try (getenv ("TEMP"), base);


  base = try ("/tmp", base);



  base = try (vartmp, base);
  base = try (usrtmp, base);
  base = try (tmp, base);


  if (base == 0)
    base = ".";



  len = strlen (base);
  tmpdir = xmalloc (len + 2);
  strcpy (tmpdir, base);
  /* Alias analysis was associating read-only memory tags to pointers
     that are not read-only.  We would then not issue any V_MAY_DEF in
     this store.  */
  tmpdir[len] = '/';
  tmpdir[len+1] = '\0';

  memoized_tmpdir = tmpdir;
  return tmpdir;
}
