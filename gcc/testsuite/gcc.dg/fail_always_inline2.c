extern char *__realpath_alias (__const char *__restrict __name, char 
*__restrict __resolved) __asm__ ("" "realpath") __attribute__ 
((__nothrow__)) __attribute__ ((__warn_unused_result__));

extern __inline __attribute__ ((__always_inline__)) __attribute__ 
((__artificial__)) __attribute__ ((__warn_unused_result__)) char *
__attribute__ ((__nothrow__)) realpath (__const char *__restrict __name, 
char *__restrict __resolved)
{
   return __realpath_alias (__name, __resolved);
}

char *
realpath(path, resolved)
  const char *path;
  char *resolved;
{
  return (((void *)0));
}
