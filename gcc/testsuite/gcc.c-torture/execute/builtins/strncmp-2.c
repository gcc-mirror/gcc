/* Copyright (C) 2000, 2001, 2003, 2005  Free Software Foundation.

   Ensure all expected transformations of builtin strncmp occur and
   perform correctly.

   Written by Kaveh R. Ghazi, 11/26/2000.  */

extern void abort (void);
typedef __SIZE_TYPE__ size_t;
extern int strncmp (const char *, const char *, size_t);

void
main_test (void)
{
#if !defined(__OPTIMIZE__) || ((defined(__sh__) || defined(__i386__) || defined (__x86_64__)) && !defined(__OPTIMIZE_SIZE__))
  /* These tests work on platforms which support cmpstrsi.  We test it
     at -O0 on all platforms to ensure the strncmp logic is correct.  */
  const char *const s1 = "hello world";
  const char *s2;
  int n = 6, x;
  
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
}
