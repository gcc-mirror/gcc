/* Copyright (C) 2000, 2001, 2003  Free Software Foundation.

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
  int n = 6, x;
  
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
  s2 = s1;
  if (strncmp (++s2, "", 1) <= 0 || s2 != s1+1)
    abort();
  if (strncmp ("", ++s2, 1) >= 0 || s2 != s1+2)
    abort();
  if (strncmp (++s2, "", 100) <= 0 || s2 != s1+3)
    abort();
  if (strncmp ("", ++s2, 100) >= 0 || s2 != s1+4)
    abort();
  if (strncmp (++s2+6, "", 100) != 0 || s2 != s1+5)
    abort();
  if (strncmp ("", ++s2+5, 100) != 0 || s2 != s1+6)
    abort();
  if (strncmp ("ozz", ++s2, 1) != 0 || s2 != s1+7)
    abort();
  if (strncmp (++s2, "rzz", 1) != 0 || s2 != s1+8)
    abort();
  s2 = s1; s3 = s1+4;
  if (strncmp (++s2, ++s3+2, 1) >= 0 || s2 != s1+1 || s3 != s1+5)
    abort();
#if !defined(__OPTIMIZE__) || (defined(__i386__) && !defined(__OPTIMIZE_SIZE__))
  /* These tests work on platforms which support cmpstrsi.  We test it
     at -O0 on all platforms to ensure the strncmp logic is correct.  */
  s2 = s1;
  if (strncmp (++s2, "ello", 3) != 0 || s2 != s1+1)
    abort();
  s2 = s1;
  if (strncmp ("ello", ++s2, 3) != 0 || s2 != s1+1)
    abort();
  s2 = s1;
  if (strncmp (++s2, "ello", 4) != 0 || s2 != s1+1)
    abort();
  s2 = s1;
  if (strncmp ("ello", ++s2, 4) != 0 || s2 != s1+1)
    abort();
  s2 = s1;
  if (strncmp (++s2, "ello", 5) <= 0 || s2 != s1+1)
    abort();
  s2 = s1;
  if (strncmp ("ello", ++s2, 5) >= 0 || s2 != s1+1)
    abort();
  s2 = s1;
  if (strncmp (++s2, "ello", 6) <= 0 || s2 != s1+1)
    abort();
  s2 = s1;
  if (strncmp ("ello", ++s2, 6) >= 0 || s2 != s1+1)
    abort();

  s2 = s1;
  if (strncmp (++s2, "zllo", 3) >= 0 || s2 != s1+1)
    abort();
  s2 = s1;
  if (strncmp ("zllo", ++s2, 3) <= 0 || s2 != s1+1)
    abort();
  s2 = s1;
  if (strncmp (++s2, "zllo", 4) >= 0 || s2 != s1+1)
    abort();
  s2 = s1;
  if (strncmp ("zllo", ++s2, 4) <= 0 || s2 != s1+1)
    abort();
  s2 = s1;
  if (strncmp (++s2, "zllo", 5) >= 0 || s2 != s1+1)
    abort();
  s2 = s1;
  if (strncmp ("zllo", ++s2, 5) <= 0 || s2 != s1+1)
    abort();
  s2 = s1;
  if (strncmp (++s2, "zllo", 6) >= 0 || s2 != s1+1)
    abort();
  s2 = s1;
  if (strncmp ("zllo", ++s2, 6) <= 0 || s2 != s1+1)
    abort();

  s2 = s1;
  if (strncmp (++s2, "allo", 3) <= 0 || s2 != s1+1)
    abort();
  s2 = s1;
  if (strncmp ("allo", ++s2, 3) >= 0 || s2 != s1+1)
    abort();
  s2 = s1;
  if (strncmp (++s2, "allo", 4) <= 0 || s2 != s1+1)
    abort();
  s2 = s1;
  if (strncmp ("allo", ++s2, 4) >= 0 || s2 != s1+1)
    abort();
  s2 = s1;
  if (strncmp (++s2, "allo", 5) <= 0 || s2 != s1+1)
    abort();
  s2 = s1;
  if (strncmp ("allo", ++s2, 5) >= 0 || s2 != s1+1)
    abort();
  s2 = s1;
  if (strncmp (++s2, "allo", 6) <= 0 || s2 != s1+1)
    abort();
  s2 = s1;
  if (strncmp ("allo", ++s2, 6) >= 0 || s2 != s1+1)
    abort();

  s2 = s1; n = 2; x = 1;
  if (strncmp (++s2, s1+(x&3), ++n) != 0 || s2 != s1+1 || n != 3)
    abort();
  s2 = s1; n = 2; x = 1;
  if (strncmp (s1+(x&3), ++s2, ++n) != 0 || s2 != s1+1 || n != 3)
    abort();
  s2 = s1; n = 3; x = 1;
  if (strncmp (++s2, s1+(x&3), ++n) != 0 || s2 != s1+1 || n != 4)
    abort();
  s2 = s1; n = 3; x = 1;
  if (strncmp (s1+(x&3), ++s2, ++n) != 0 || s2 != s1+1 || n != 4)
    abort();
  s2 = s1; n = 4; x = 1;
  if (strncmp (++s2, s1+(x&3), ++n) != 0 || s2 != s1+1 || n != 5)
    abort();
  s2 = s1; n = 4; x = 1;
  if (strncmp (s1+(x&3), ++s2, ++n) != 0 || s2 != s1+1 || n != 5)
    abort();
  s2 = s1; n = 5; x = 1;
  if (strncmp (++s2, s1+(x&3), ++n) != 0 || s2 != s1+1 || n != 6)
    abort();
  s2 = s1; n = 5; x = 1;
  if (strncmp (s1+(x&3), ++s2, ++n) != 0 || s2 != s1+1 || n != 6)
    abort();

  s2 = s1; n = 2;
  if (strncmp (++s2, "zllo", ++n) >= 0 || s2 != s1+1 || n != 3)
    abort();
  s2 = s1; n = 2; x = 1;
  if (strncmp ("zllo", ++s2, ++n) <= 0 || s2 != s1+1 || n != 3)
    abort();
  s2 = s1; n = 3; x = 1;
  if (strncmp (++s2, "zllo", ++n) >= 0 || s2 != s1+1 || n != 4)
    abort();
  s2 = s1; n = 3; x = 1;
  if (strncmp ("zllo", ++s2, ++n) <= 0 || s2 != s1+1 || n != 4)
    abort();
  s2 = s1; n = 4; x = 1;
  if (strncmp (++s2, "zllo", ++n) >= 0 || s2 != s1+1 || n != 5)
    abort();
  s2 = s1; n = 4; x = 1;
  if (strncmp ("zllo", ++s2, ++n) <= 0 || s2 != s1+1 || n != 5)
    abort();
  s2 = s1; n = 5; x = 1;
  if (strncmp (++s2, "zllo", ++n) >= 0 || s2 != s1+1 || n != 6)
    abort();
  s2 = s1; n = 5; x = 1;
  if (strncmp ("zllo", ++s2, ++n) <= 0 || s2 != s1+1 || n != 6)
    abort();

  s2 = s1; n = 2;
  if (strncmp (++s2, "allo", ++n) <= 0 || s2 != s1+1 || n != 3)
    abort();
  s2 = s1; n = 2; x = 1;
  if (strncmp ("allo", ++s2, ++n) >= 0 || s2 != s1+1 || n != 3)
    abort();
  s2 = s1; n = 3; x = 1;
  if (strncmp (++s2, "allo", ++n) <= 0 || s2 != s1+1 || n != 4)
    abort();
  s2 = s1; n = 3; x = 1;
  if (strncmp ("allo", ++s2, ++n) >= 0 || s2 != s1+1 || n != 4)
    abort();
  s2 = s1; n = 4; x = 1;
  if (strncmp (++s2, "allo", ++n) <= 0 || s2 != s1+1 || n != 5)
    abort();
  s2 = s1; n = 4; x = 1;
  if (strncmp ("allo", ++s2, ++n) >= 0 || s2 != s1+1 || n != 5)
    abort();
  s2 = s1; n = 5; x = 1;
  if (strncmp (++s2, "allo", ++n) <= 0 || s2 != s1+1 || n != 6)
    abort();
  s2 = s1; n = 5; x = 1;
  if (strncmp ("allo", ++s2, ++n) >= 0 || s2 != s1+1 || n != 6)
    abort();

#endif  
  
  /* Test at least one instance of the __builtin_ style.  We do this
     to ensure that it works and that the prototype is correct.  */
  if (__builtin_strncmp ("hello", "a", 100) <= 0)
    abort();

  return 0;
}

#ifdef __OPTIMIZE__
/* When optimizing, all the above cases should be transformed into
   something else.  So any remaining calls to the original function
   should abort.  */
__attribute__ ((noinline))
static int
strncmp(const char *s1, const char *s2, size_t n)
{
  abort();
}
#endif
