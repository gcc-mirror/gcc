/* Copyright (C) 2000  Free Software Foundation.

   Ensure all expected transformations of builtin strncmp occur and
   perform correctly.

   Written by Kaveh R. Ghazi, 11/26/2000.  */

extern void abort (void);
typedef __SIZE_TYPE__ size_t;
extern int strncmp (const char *, const char *, size_t);

int main ()
{
  const char *const s1 = "hello world";
  const char *s2, *s3;
  
  if (strncmp (s1, "hello world", 12) != 0)
    abort();
  if (strncmp ("hello world", s1, 12) != 0)
    abort();
  if (strncmp ("hello", "hello", 6) != 0)
    abort();
  if (strncmp ("hello", "hello", 2) != 0)
    abort();
  if (strncmp ("hello", "hello", 100) != 0)
    abort();
  if (strncmp (s1+10, "d", 100) != 0)
    abort();
  if (strncmp (10+s1, "d", 100) != 0)
    abort();
  if (strncmp ("d", s1+10, 1) != 0)
    abort();
  if (strncmp ("d", 10+s1, 1) != 0)
    abort();
  if (strncmp ("hello", "aaaaa", 100) <= 0)
    abort();
  if (strncmp ("aaaaa", "hello", 100) >= 0)
    abort();
  if (strncmp ("hello", "aaaaa", 1) <= 0)
    abort();
  if (strncmp ("aaaaa", "hello", 1) >= 0)
    abort();

  s2 = s1; s3 = s1+4;
  if (strncmp (++s2, ++s3, 0) != 0 || s2 != s1+1 || s3 != s1+5)
    abort();
  
  return 0;
}

#ifdef __OPTIMIZE__
/* When optimizing, all the above cases should be transformed into
   something else.  So any remaining calls to the original function
   should abort.  */
static char *
strncmp(const char *s1, const char *s2, size_t n)
{
  abort();
}
#endif
