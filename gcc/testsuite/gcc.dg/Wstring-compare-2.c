/* PR tree-optimization/90879 - fold zero-equality of strcmp between
   a longer string and a smaller array
   Test for a warning for strcmp of a longer string against smaller
   array.
   { dg-do compile }
   { dg-options "-O2 -Wall -Wstring-compare -Wno-stringop-truncation -ftrack-macro-expansion=0" } */

typedef __SIZE_TYPE__ size_t;

extern void* memcpy (void*, const void*, size_t);

extern int strcmp (const char*, const char*);
extern size_t strlen (const char*);
extern char* strcpy (char*, const char*);
extern char* strncpy (char*, const char*, size_t);
extern int strncmp (const char*, const char*, size_t);

void sink (int, ...);
#define sink(...) sink (__LINE__, __VA_ARGS__)


extern char a1[1], a2[2], a3[3], a4[4], a5[5], a6[6], a7[7], a8[8], a9[9];

#define T(a, b) sink (0 == strcmp (a, b))


void test_string_cst (void)
{
  const char *s1 = "1", *s2 = "12";

  T (s1, a1);                 // { dg-warning ".strcmp. of a string of length 1 and an array of size 1 evaluates to nonzero" }
  T (s1, a2);
  T (s1, a3);

  T (a1, s1);                 // { dg-warning ".strcmp. of a string of length 1 and an array of size 1 evaluates to nonzero" }
  T (a2, s1);
  T (a3, s1);

  T (s2, a1);                 // { dg-warning ".strcmp. of a string of length 2 and an array of size 1 evaluates to nonzero" }
  T (s2, a2);                 // { dg-warning ".strcmp. of a string of length 2 and an array of size 2 evaluates to nonzero" }
  T (s2, a3);

  T (a1, s2);                 // { dg-warning ".strcmp. of a string of length 2 and an array of size 1 evaluates to nonzero" }
  T (a2, s2);                 // { dg-warning ".strcmp. of a string of length 2 and an array of size 2 evaluates to nonzero" }
  T (a3, s2);
}


void test_string_cst_off_cst (void)
{
  const char *s1 = "1", *s2 = "12", *s3 = "123", *s4 = "1234";

  T (s1, a2 + 1);              // { dg-warning ".strcmp. of a string of length 1 and an array of size 1 evaluates to nonzero" }
  T (a2 + 1, s1);              // { dg-warning ".strcmp. of a string of length 1 and an array of size 1 evaluates to nonzero" }


  T (s3 + 1, a2);             // { dg-warning ".strcmp. of a string of length 2 and an array of size 2 evaluates to nonzero" }
  T (s3 + 1, a3);

  T (s2, a4 + 1);
  T (s2, a4 + 2);             // { dg-warning ".strcmp. of a string of length 2 and an array of size 2 evaluates to nonzero" }

  T (s4, a4 + 1);             // { dg-warning ".strcmp. of a string of length 4 and an array of size 3 evaluates to nonzero" }
  T (s3, a5 + 1);
}


/* Use strncpy below rather than memcpy until PR 91183 is resolved.  */

#undef T
#define T(s, n, a)					\
  do {							\
    char arr[32];					\
    sink (arr);						\
    strncpy (arr, s, n < 0 ? strlen (s) + 1: n);	\
    sink (0 == strcmp (arr, a));			\
  } while (0)

void test_string_exact_length (void)
{
  const char *s1 = "1", *s2 = "12";

  T (s1, -1, a1);             // { dg-warning ".strcmp. of a string of length 1 and an array of size 1 evaluates to nonzero" }
  T (s1, -1, a2);
  T (s1, -1, a3);

  T (s2, -1, a1);             // { dg-warning ".strcmp. of a string of length 2 and an array of size 1 evaluates to nonzero" }
  T (s2, -1, a2);             // { dg-warning ".strcmp. of a string of length 2 and an array of size 2 evaluates to nonzero" }
  T (s2, -1, a3);
}


void test_string_min_length (void)
{
  const char *s1 = "1", *s2 = "12";

  T (s1,  1, a1);             // { dg-warning ".strcmp. of a string of length 1 or more and an array of size 1 evaluates to nonzero" }
  T (s1,  1, a2);
  T (s1,  1, a3);

  T (s2,  2, a1);             // { dg-warning ".strcmp. of a string of length 2 or more and an array of size 1 evaluates to nonzero" }
  T (s2,  2, a2);             // { dg-warning ".strcmp. of a string of length 2 or more and an array of size 2 evaluates to nonzero" }
  T (s2,  2, a3);
}


int test_strncmp_str_lit_var (const char *s, long n)
{
  if (strncmp (s, "123456", n) == 0)    // { dg-bogus "\\\[-Wstring-compare" }
    return 1;

  return 0;
}

int test_strlen_strncmp_str_lit_var (const char *s, long n)
{
  if (__builtin_strlen (s) < n)
    return -1;

  if (n == 6)
    if (strncmp (s, "123456", n) == 0)  // { dg-bogus "\\\[-Wstring-compare" }
      return 1;

  return 0;
}


