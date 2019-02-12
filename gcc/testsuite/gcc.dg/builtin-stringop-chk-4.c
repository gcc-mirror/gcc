/* Test exercising buffer overflow warnings emitted for raw memory and
   string manipulation builtins involving ranges of sizes and strings
   of varying lengths.  */
/* { dg-do compile } */
/* { dg-options "-O2 -ftrack-macro-expansion=0" } */

#define INT_MAX      __INT_MAX__
#define PTRDIFF_MAX  __PTRDIFF_MAX__
#define SIZE_MAX     __SIZE_MAX__

typedef __PTRDIFF_TYPE__ ptrdiff_t;
typedef __SIZE_TYPE__    size_t;

static const size_t ssize_max = SIZE_MAX / 2;
static const size_t size_max = SIZE_MAX;

extern signed char    schar_val;
extern signed short   sshrt_val;
extern signed int     sint_val;
extern signed long    slong_val;
extern unsigned char  uchar_val;
extern unsigned short ushrt_val;
extern unsigned int   uint_val;
extern unsigned long  ulong_val;

#define memcpy(d, s, n) (memcpy ((d), (s), (n)), sink ((d)))
extern void* (memcpy)(void*, const void*, size_t);

#define mempcpy(d, s, n) (mempcpy ((d), (s), (n)), sink ((d)))
extern void* (mempcpy)(void*, const void*, size_t);

#define memset(d, c, n) (memset ((d), (c), (n)), sink ((d)))
extern void* (memset)(void*, int, size_t);

#define bzero(d, n) (bzero ((d), (n)), sink ((d)))
extern void (bzero)(void*, size_t);

#define strcat(d, s) (strcat ((d), (s)), sink ((d)))
extern char* (strcat)(char*, const char*);

#define strncat(d, s, n) (strncat ((d), (s), (n)), sink ((d)))
extern char* (strncat)(char*, const char*, size_t);

#define strcpy(d, s) (strcpy ((d), (s)), sink ((d)))
extern char* (strcpy)(char*, const char*);

#define strncpy(d, s, n) (strncpy ((d), (s), (n)), sink ((d)))
extern char* (strncpy)(char*, const char*, size_t);

void sink (void*);

/* Function to "generate" a random number each time it's called.  Declared
   (but not defined) and used to prevent GCC from making assumptions about
   their values based on the variables uses in the tested expressions.  */
size_t random_unsigned_value (void);
ptrdiff_t random_signed_value (void);

/* Return a random unsigned value between MIN and MAX.  */

static inline size_t
unsigned_range (size_t min, size_t max)
{
  const size_t val = random_unsigned_value ();
  return val < min || max < val ? min : val;
}

/* Return a random signed value between MIN and MAX.  */

static inline ptrdiff_t
signed_range (ptrdiff_t min, ptrdiff_t max)
{
  const ptrdiff_t val = random_signed_value ();
  return val < min || max < val ? min : val;
}

/* For brevity.  */
#define UR(min, max)   unsigned_range (min, max)
#define SR(min, max)   signed_range (min, max)

/* Return a pointer to constant string whose length is at least MINLEN
   and at most 10.  */
#define S(minlen)				\
  (minlen == random_unsigned_value ()		\
   ? "0123456789" + 10 - minlen : "0123456789")

/* Test memcpy with a number of bytes bounded by a known range.  */

void test_memcpy_range (void *d, const void *s)
{
  char buf[5];

  memcpy (buf, s, UR (0, 5));
  memcpy (buf, s, UR (1, 5));
  memcpy (buf, s, UR (2, 5));
  memcpy (buf, s, UR (3, 5));
  memcpy (buf, s, UR (4, 5));

  memcpy (buf, s, UR (6, 7));  /* { dg-warning "writing between 6 and 7 bytes into a region of size 5 overflows the destination" } */

  memcpy (buf + 5, s, UR (1, 2));  /* { dg-warning "writing between 1 and 2 bytes into a region of size 0 overflows the destination" } */

  memcpy (buf + size_max, s, UR (1, 2));  /* { dg-warning "writing between 1 and 2 bytes into a region of size 0 overflows the destination" "excessive pointer offset" { xfail *-*-* } } */

  memcpy (buf, s, UR (ssize_max, size_max));   /* { dg-warning "writing \[0-9\]+ or more bytes into a region of size 5 overflows the destination" } */
  memcpy (buf, s, UR (ssize_max + 1, size_max));  /* { dg-warning "specified \(bound|size\) between \[0-9\]+ and \[0-9\]+ exceeds maximum object size" } */
  memcpy (buf, s, UR (size_max - 1, size_max));  /* { dg-warning "specified \(bound|size\) between \[0-9\]+ and \[0-9\]+ exceeds maximum object size" } */

  /* Exercise memcpy into a destination of unknown size with excessive
     number of bytes.  */
  memcpy (d, s, UR (ssize_max, size_max));
  memcpy (d, s, UR (ssize_max + 1, size_max));   /* { dg-warning "specified \(bound|size\) between \[0-9\]+ and \[0-9\]+ exceeds maximum object size" } */

  memcpy (buf, s, SR (-1, 1));
  memcpy (buf, s, SR (-3, 2));
  memcpy (buf, s, SR (-5, 3));
  memcpy (buf, s, SR (-7, 4));
  memcpy (buf, s, SR (-9, 5));
  memcpy (buf, s, SR (-11, 6));

  memcpy (d, s, SR (-1, 1));
  memcpy (d, s, SR (-3, 2));
  memcpy (d, s, SR (-5, 3));
  memcpy (d, s, SR (-7, 4));
  memcpy (d, s, SR (-9, 5));
  memcpy (d, s, SR (-11, 6));

  memcpy (buf, s, SR (-2, -1));   /* { dg-warning "specified \(bound|size\) between \[0-9\]+ and \[0-9\]+ exceeds maximum object size" } */
  memcpy (d, s, SR (-2, -1));   /* { dg-warning "specified \(bound|size\) between \[0-9\]+ and \[0-9\]+ exceeds maximum object size" } */

  /* Even though the following calls are bounded by the range of N's
     type they must not cause a warning for obvious reasons.  */
  memcpy (buf, s, schar_val);
  memcpy (buf, s, sshrt_val);
  memcpy (buf, s, sint_val);
  memcpy (buf, s, slong_val);

  memcpy (buf, s, uchar_val);
  memcpy (buf, s, ushrt_val);
  memcpy (buf, s, uint_val);
  memcpy (buf, s, ulong_val);

  memcpy (buf, s, schar_val + 1);
  memcpy (buf, s, sshrt_val + 2);
  memcpy (buf, s, sint_val + 3);
  memcpy (buf, s, slong_val + 4);

  memcpy (d, s, uchar_val + 5);
  memcpy (d, s, ushrt_val + 6);
  memcpy (d, s, uint_val + 7);
  memcpy (d, s, ulong_val + 8);

  memcpy (d, s, schar_val);
  memcpy (d, s, sshrt_val);
  memcpy (d, s, sint_val);
  memcpy (d, s, slong_val);

  memcpy (d, s, uchar_val);
  memcpy (d, s, ushrt_val);
  memcpy (d, s, uint_val);
  memcpy (d, s, ulong_val);

  memcpy (d, s, schar_val + 1);
  memcpy (d, s, sshrt_val + 2);
  memcpy (d, s, sint_val + 3);
  memcpy (d, s, slong_val + 4);

  memcpy (d, s, uchar_val + 5);
  memcpy (d, s, ushrt_val + 6);
  memcpy (d, s, uint_val + 7);
  memcpy (d, s, ulong_val + 8);
}

/* Test mempcpy with a number of bytes bounded by a known range.  */

void test_mempcpy_range (void *d, const void *s)
{
  char buf[5];

  mempcpy (buf, s, UR (0, 5));
  mempcpy (buf, s, UR (1, 5));
  mempcpy (buf, s, UR (2, 5));
  mempcpy (buf, s, UR (3, 5));
  mempcpy (buf, s, UR (4, 5));

  mempcpy (buf, s, UR (6, 7));  /* { dg-warning "writing between 6 and 7 bytes into a region of size 5 overflows the destination" } */

  mempcpy (buf, s, UR (6, 7));  /* { dg-warning "writing between 6 and 7 bytes into a region of size 5 overflows the destination" } */

  mempcpy (buf, s, UR (ssize_max, size_max));   /* { dg-warning "writing \[0-9\]+ or more bytes into a region of size 5 overflows the destination" } */
  mempcpy (buf, s, UR (ssize_max + 1, size_max));  /* { dg-warning "specified \(bound|size\) between \[0-9\]+ and \[0-9\]+ exceeds maximum object size" } */
  mempcpy (buf, s, UR (size_max - 1, size_max));  /* { dg-warning "specified \(bound|size\) between \[0-9\]+ and \[0-9\]+ exceeds maximum object size" } */

  /* Exercise mempcpy into a destination of unknown size with excessive
     number of bytes.  */
  mempcpy (d, s, UR (ssize_max, size_max));
  mempcpy (d, s, UR (ssize_max + 1, size_max));   /* { dg-warning "specified \(bound|size\) between \[0-9\]+ and \[0-9\]+ exceeds maximum object size" } */
}

/* Test memset with a number of bytes bounded by a known range.  */

void test_memset_range (void *d)
{
  char buf[5];

  memset (buf, 0, UR (0, 5));
  memset (buf, 0, UR (1, 5));
  memset (buf, 0, UR (2, 5));
  memset (buf, 0, UR (3, 5));
  memset (buf, 0, UR (4, 5));

  memset (buf, 0, UR (6, 7));  /* { dg-warning "writing between 6 and 7 bytes into a region of size 5 overflows the destination" } */

  memset (buf, 0, UR (6, 7));  /* { dg-warning "writing between 6 and 7 bytes into a region of size 5 overflows the destination" } */

  memset (buf, 0, UR (ssize_max, size_max));   /* { dg-warning "writing \[0-9\]+ or more bytes into a region of size 5 overflows the destination" } */
  memset (buf, 0, UR (ssize_max + 1, size_max));  /* { dg-warning "specified \(bound|size\) between \[0-9\]+ and \[0-9\]+ exceeds maximum object size" } */
  memset (buf, 0, UR (size_max - 1, size_max));  /* { dg-warning "specified \(bound|size\) between \[0-9\]+ and \[0-9\]+ exceeds maximum object size" } */

  /* Exercise memset into a destination of unknown size with excessive
     number of bytes.  */
  memset (d, 0, UR (ssize_max, size_max));
  memset (d, 0, UR (ssize_max + 1, size_max));   /* { dg-warning "specified \(bound|size\) between \[0-9\]+ and \[0-9\]+ exceeds maximum object size" } */
}

/* Test bzero with a number of bytes bounded by a known range.  */

void test_bzero_range (void *d)
{
  char buf[5];

  bzero (buf, UR (0, 5));
  bzero (buf, UR (1, 5));
  bzero (buf, UR (2, 5));
  bzero (buf, UR (3, 5));
  bzero (buf, UR (4, 5));

  bzero (buf, UR (6, 7));  /* { dg-warning "writing between 6 and 7 bytes into a region of size 5 overflows the destination" } */

  bzero (buf, UR (6, 7));  /* { dg-warning "writing between 6 and 7 bytes into a region of size 5 overflows the destination" } */

  bzero (buf, UR (ssize_max, size_max));   /* { dg-warning "writing \[0-9\]+ or more bytes into a region of size 5 overflows the destination" } */
  bzero (buf, UR (ssize_max + 1, size_max));  /* { dg-warning "specified \(bound|size\) between \[0-9\]+ and \[0-9\]+ exceeds maximum object size" } */
  bzero (buf, UR (size_max - 1, size_max));  /* { dg-warning "specified \(bound|size\) between \[0-9\]+ and \[0-9\]+ exceeds maximum object size" } */

  /* Exercise bzero into a destination of unknown size with excessive
     number of bytes.  */
  bzero (d, UR (ssize_max, size_max));
  bzero (d, UR (ssize_max + 1, size_max));   /* { dg-warning "specified \(bound|size\) between \[0-9\]+ and \[0-9\]+ exceeds maximum object size" } */
}

/* Test strcat with an argument referencing a non-constant string of
   lengths in a known range.  */

void test_strcat_range (void)
{
  char buf[5] = "";

  strcat (buf, S (0));
  strcat (buf, S (1));
  strcat (buf, S (2));
  strcat (buf, S (3));
  strcat (buf, S (4));
  strcat (buf, S (5));   /* { dg-warning "writing between 6 and 11 bytes into a region of size 5 " } */

  {
    /* The implementation of the warning isn't smart enough to determine
       the length of the string in the buffer so it assumes it's empty
       and issues the warning basically for the same cases as strcat.  */
    char buf2[5] = "12";
    strcat (buf2, S (4));   /* { dg-warning "writing 5 bytes into a region of size 3" "strcat to a non-empty string" { xfail *-*-* } } */
  }
}

/* Verify that strcpy with an unknown source string doesn't cause
   warnings unless the destination has zero size.  */

void test_strcpy (const char *src)
{
  struct A { char a[2]; char b[3]; } a;

  strcpy (a.a, src);
  strcpy (a.a + 1, src);

  /* There must be enough room in the destination for the terminating
     nul, otherwise verify that a warning is issued.
     The following works as expected with __builtin___strcpy_chk and
     __builtin_object_size because they see that the offset is from
     the a.a array.  When optimization is enabled, it isn't detected
     by __bultin_strcpy (when __builtin_object_size isn't called
     explicitly) because by the time it's seen the offset has been
     transformed to one from the beginning of the whole object, i.e.,
     as if it had been written as (char*)&a + 2 .  Then the destination
     size is taken to be the rest of the whole object.  It is detected
     by __builtin_strcpy when optimization is not enabled because then
     the &a.a + 2 expression is preserved.  But without optimization
     an ordinary call to strcpy isn't transformed to __builtin_strcpy
     and so it can't be detected here (since the rest of the test
     relies on optimization).  */
  strcpy (a.a + 2, src);    /* { dg-warning "writing at least 1 byte into a region of size 0 " "strcpy into empty substring" { xfail *-*-* } } */

  /* This does work.  */
  strcpy (a.a + 5, src);    /* { dg-warning "writing 1 or more bytes into a region of size 0 " } */

  /* As does this.  */
  strcpy (a.a + 17, src);    /* { dg-warning "writing 1 or more bytes into a region of size 0 " } */
}

/* Test strcpy with a non-constant source string of length in a known
   range.  */

void test_strcpy_range (void)
{
  char buf[5];

  strcpy (buf, S (0));
  strcpy (buf, S (1));
  strcpy (buf, S (2));
  strcpy (buf, S (4));
  strcpy (buf, S (5));   /* { dg-warning "writing between 6 and 11 bytes into a region of size 5 " } */
  strcpy (buf, S (6));   /* { dg-warning "writing between 7 and 11 bytes" } */
  strcpy (buf, S (7));   /* { dg-warning "writing between 8 and 11 bytes" } */
  strcpy (buf, S (8));   /* { dg-warning "writing between 9 and 11 bytes" } */
  strcpy (buf, S (9));   /* { dg-warning "writing between 10 and 11 bytes" } */
  strcpy (buf, S (10));   /* { dg-warning "writing 11 bytes" } */

  strcpy (buf + 5, S (0));   /* { dg-warning "writing between 1 and 11 bytes" } */

  strcpy (buf + 17, S (0));   /* { dg-warning "writing between 1 and 11 bytes " } */
}

/* Test strncat with an argument referencing a non-constant string of
   lengths in a known range.  */

void test_strncat_range (void)
{
  char buf[5] = "";

  strncat (buf, S (0), 0);
  strncat (buf, S (0), 1);
  strncat (buf, S (0), 2);
  strncat (buf, S (0), 3);
  strncat (buf, S (0), 4);

  strncat (buf + 5, S (0), 0);

  strncat (buf + 5, S (0), 1);   /* { dg-warning "specified \(bound|size\) 1 exceeds destination size 0" } */
  strncat (buf + 5, S (1), 1);   /* { dg-warning "specified \(bound|size\) 1 exceeds destination size 0" } */

  /* Strncat always appends a terminating null after copying the N
     characters so the following triggers a warning pointing out
     that specifying sizeof(buf) as the upper bound may cause
     the nul to overflow the destination.  */
  strncat (buf, S (0), 5);   /* { dg-warning "specified \(bound|size\) 5 equals destination size" } */
  strncat (buf, S (0), 6);   /* { dg-warning "specified \(bound|size\) 6 exceeds destination size 5" } */

  strncat (buf, S (1), 0);
  strncat (buf, S (1), 1);
  strncat (buf, S (1), 2);
  strncat (buf, S (1), 3);
  strncat (buf, S (1), 4);
  strncat (buf, S (1), 5);   /* { dg-warning "specified \(bound|size\) 5 equals destination size" } */
  strncat (buf, S (1), 6);   /* { dg-warning "specified \(bound|size\) 6 exceeds destination size 5" } */
  strncat (buf, S (2), 6);   /* { dg-warning "specified \(bound|size\) 6 exceeds destination size 5" } */

  /* The following could just as well say "writing 6 bytes into a region
     of size 5.  Either would be correct and probably equally as clear
     in this case.  But when the length of the source string is not known
     at all then the bound warning seems clearer.  */
  strncat (buf, S (5), 6);   /* { dg-warning "specified \(bound|size\) 6 exceeds destination size 5" } */
  strncat (buf, S (7), 6);   /* { dg-warning "specified \(bound|size\) 6 exceeds destination size 5" } */

  {
    /* The implementation of the warning isn't smart enough to determine
       the length of the string in the buffer so it assumes it's empty
       and issues the warning basically for the same cases as strncpy.  */
    char buf2[5] = "12";
    strncat (buf2, S (4), 4);   /* { dg-warning "writing 5 bytes into a region of size 3" "strncat to a non-empty string" { xfail *-*-* } } */
  }
}

/* Test strncat_chk with an argument referencing a non-constant string
   of lengths in a known range.  */

void test_strncat_chk_range (char *d)
{
  char buf[5] = "";

#define strncat_chk(d, s, n) \
  __builtin___strncat_chk ((d), (s), (n), __builtin_object_size (d, 1));

  strncat_chk (buf, S (0), 1);
  strncat_chk (buf, S (0), 2);
  strncat_chk (buf, S (0), 3);
  strncat_chk (buf, S (0), 4);
  strncat_chk (buf, S (0), 5);   /* { dg-warning "specified \(bound|size\) 5 equals destination size" } */

  strncat_chk (buf, S (5), 1);
  strncat_chk (buf, S (5), 2);
  strncat_chk (buf, S (5), 3);
  strncat_chk (buf, S (5), 4);
  strncat_chk (buf, S (5), 5);   /* { dg-warning "specified \(bound|size\) 5 equals destination size" } */

  strncat_chk (buf, S (5), 10);   /* { dg-warning "specified \(bound|size\) \[0-9\]+ exceeds destination size 5" } */

  strncat_chk (d, S (5), size_max);   /* { dg-warning "specified \(bound|size\) \[0-9\]+ exceeds maximum object size " } */
}

/* Test strncpy with a non-constant source string of length in a known
   range and a constant number of bytes.  */

void test_strncpy_string_range (char *d)
{
  char buf[5];

  strncpy (buf, S (0), 0);
  strncpy (buf, S (0), 1);
  strncpy (buf, S (0), 2);
  strncpy (buf, S (0), 3);
  strncpy (buf, S (0), 4);
  strncpy (buf, S (0), 5);
  strncpy (buf, S (0), 6);   /* { dg-warning "writing 6 bytes into a region of size 5 " } */

  strncpy (buf, S (6), 4);
  strncpy (buf, S (7), 5);
  strncpy (buf, S (8), 6);   /* { dg-warning "writing 6 bytes into a region of size 5 " } */

  strncpy (buf, S (1), ssize_max - 1);   /* { dg-warning "writing \[0-9\]+ bytes into a region of size 5" } */
  strncpy (buf, S (2), ssize_max);   /* { dg-warning "writing \[0-9\]+ bytes into a region of size 5" } */
  strncpy (buf, S (3), ssize_max + 1);   /* { dg-warning "specified \(bound|size\) \[0-9\]+ exceeds maximum object size" } */
  strncpy (buf, S (4), size_max);   /* { dg-warning "specified \(bound|size\) \[0-9\]+ exceeds maximum object size" } */

  /* Exercise strncpy into a destination of unknown size with a valid
     and invalid constant number of bytes.  */
  strncpy (d, S (1), ssize_max - 1);
  strncpy (d, S (2), ssize_max);
  strncpy (d, S (3), ssize_max + 1);   /* { dg-warning "specified \(bound|size\) \[0-9\]+ exceeds maximum object size" } */
  strncpy (d, S (4), size_max);   /* { dg-warning "specified \(bound|size\) \[0-9\]+ exceeds maximum object size" } */
}

/* Test strncpy with a non-constant source string of length in a known
   range and a non-constant number of bytes also in a known range.  */

void test_strncpy_string_count_range (char *dst, const char *src)
{
  char buf[5];

  strncpy (buf, S (0), UR (0, 1));
  strncpy (buf, S (0), UR (0, 2));
  strncpy (buf, S (0), UR (0, 3));
  strncpy (buf, S (0), UR (0, 4));
  strncpy (buf, S (0), UR (0, 5));
  strncpy (buf, S (0), UR (0, 6));
  strncpy (buf, S (0), UR (1, 6));
  strncpy (buf, S (0), UR (2, 6));
  strncpy (buf, S (0), UR (3, 6));
  strncpy (buf, S (0), UR (4, 6));
  strncpy (buf, S (0), UR (5, 6));

  strncpy (buf, S (9), UR (0, 1));
  strncpy (buf, S (8), UR (0, 2));
  strncpy (buf, S (7), UR (0, 3));
  strncpy (buf, S (6), UR (0, 4));
  strncpy (buf, S (8), UR (0, 5));
  strncpy (buf, S (7), UR (0, 6));
  strncpy (buf, S (6), UR (1, 6));
  strncpy (buf, S (5), UR (2, 6));
  strncpy (buf, S (9), UR (3, 6));
  strncpy (buf, S (8), UR (4, 6));
  strncpy (buf, S (7), UR (5, 6));

  strncpy (buf, S (0), UR (6, 7));   /* { dg-warning "writing between 6 and 7 bytes into a region of size 5 " } */
  strncpy (buf, S (1), UR (7, 8));   /* { dg-warning "writing between 7 and 8 bytes into a region of size 5 " } */
  strncpy (buf, S (2), UR (ssize_max, ssize_max + 1));   /* { dg-warning "writing \[0-9\]+ or more bytes into a region of size 5 " } */

  strncpy (buf, S (2), UR (ssize_max + 1, ssize_max + 2));   /* { dg-warning "specified \(bound|size\) between \[0-9\]+ and \[0-9\]+ exceeds maximum object size" } */

  strncpy (buf + 5, S (0), UR (0, 1));
  strncpy (buf + 5, S (1), UR (0, 1));
  strncpy (buf + 5, S (0), UR (1, 2));   /* { dg-warning "writing between 1 and 2 bytes into a region of size 0 " } */
  strncpy (buf + 5, S (1), UR (1, 2));   /* { dg-warning "writing between 1 and 2 bytes into a region of size 0 " } */

  strncpy (buf, src, UR (0, 1));
  strncpy (buf, src, UR (0, 2));
  strncpy (buf, src, UR (0, 3));
  strncpy (buf, src, UR (0, 4));
  strncpy (buf, src, UR (0, 5));
  strncpy (buf, src, UR (0, 6));
  strncpy (buf, src, UR (1, 6));
  strncpy (buf, src, UR (2, 6));
  strncpy (buf, src, UR (3, 6));
  strncpy (buf, src, UR (4, 6));
  strncpy (buf, src, UR (5, 6));
  strncpy (buf, src, UR (6, 7));   /* { dg-warning "writing between 6 and 7 bytes into a region of size 5 " } */

  /* Exercise strncpy into a destination of unknown size  with a valid
     and invalid constant number of bytes.  */
  strncpy (dst, S (0), UR (5, 6));
  strncpy (dst, S (1), UR (6, 7));
  strncpy (dst, S (2), UR (7, 8));

  strncpy (dst, S (3), UR (ssize_max, ssize_max + 1));

  strncpy (dst, S (4), UR (ssize_max + 1, ssize_max + 2));   /* { dg-warning "specified \(bound|size\) between \[0-9\]+ and \[0-9\]+ exceeds maximum object size" } */
}
