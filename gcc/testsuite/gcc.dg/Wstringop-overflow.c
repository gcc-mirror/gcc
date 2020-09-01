/* PR middle-end/77608 - missing protection on trivially detectable runtime
   buffer overflow
   { dg-do compile }
   { dg-options "-O2 -Wstringop-overflow -ftrack-macro-expansion=0" }  */

#define SIZE_MAX   __SIZE_MAX__
#define DIFF_MAX   __PTRDIFF_MAX__
#define DIFF_MIN   (-DIFF_MAX - 1)

typedef __SIZE_TYPE__ size_t;

extern void* memcpy (void*, const void*, size_t);
extern char* strcpy (char*, const char*);
extern char* strncpy (char*, const char*, size_t);

void sink (void*);

static size_t unsigned_value (void)
{
  extern volatile size_t unsigned_value_source;
  return unsigned_value_source;
}

static size_t unsigned_range (size_t min, size_t max)
{
  size_t val = unsigned_value ();
  return val < min || max < val ? min : val;
}

#define UR(min, max) unsigned_range (min, max)


char a7[7];

struct MemArray { char a9[9]; char a1[1]; };

void test_memcpy_array (const void *s)
{
#define T(d, s, n) (memcpy ((d), (s), (n)), sink (d))

  T (a7 + UR (0, 1), s, 7);
  T (a7 + UR (0, 7), s, 7);
  T (a7 + UR (0, 8), s, 7);
  T (a7 + UR (0, DIFF_MAX), s, 7);
  T (a7 + UR (0, SIZE_MAX), s, 7);

  T (a7 + UR (1, 2), s, 7);   /* { dg-warning "writing 7 bytes into a region of size 6" } */
  T (a7 + UR (2, 3), s, 7);   /* { dg-warning "writing 7 bytes into a region of size 5" } */
  T (a7 + UR (6, 9), s, 7);   /* { dg-warning "writing 7 bytes into a region of size 1" } */
  T (a7 + UR (7, 9), s, 7);   /* { dg-warning "writing 7 bytes into a region of size 0" } */
  T (a7 + UR (8, 9), s, 7);   /* { dg-warning "writing 7 bytes into a region of size 0" } */

  T (a7 + UR (9, 10), s, 7);  /* { dg-warning "writing 7 bytes into a region of size 0" } */
  T (a7 + UR (DIFF_MAX, DIFF_MAX + (size_t)1), s, 7);  /* { dg-warning "writing 7 bytes into a region of size 0" } */
  T (a7 + UR (DIFF_MAX, SIZE_MAX), s, 7);  /* { dg-warning "writing 7 bytes into a region of size 0" } */

  /* This is valid.  */
  char *d = a7 + 7;
  T (d + UR (-8, -7), s, 7);
}

/* Verify the absence of warnings for memcpy writing beyond object
   boundaries. */

void test_memcpy_memarray (struct MemArray *p, const void *s)
{
#undef T
#define T(d, s, n) (memcpy ((d), (s), (n)), sink (d))

  /* The following are valid.  */
  T (p->a9 + UR (0, 1), s, 9);
  T (p->a9 + UR (0, 7), s, 9);
  T (p->a9 + UR (0, 8), s, 9);
  T (p->a9 + UR (0, DIFF_MAX), s, 9);
  T (p->a9 + UR (0, SIZE_MAX), s, 9);

  /* The following are invalid.  Unfortunately, there is apparently enough
     code out there that abuses memcpy to write past the end of one member
     and into the members that follow so the following are not diagnosed
     by design.  It sure would be nice not to have to cater to hacks like
     these...  */
  T (p->a9 + UR (1, 2), s, 9);
  T (p->a9 + UR (1, 2), s, 123);
}


void test_strcpy_array (void)
{
#undef T
#define T(d, s) (strcpy ((d), (s)), sink (d))

  T (a7 + UR (0, 1), "012345");
  T (a7 + UR (0, 7), "012345");
  T (a7 + UR (0, 8), "012345");
  T (a7 + UR (0, DIFF_MAX), "012345");
  T (a7 + UR (0, SIZE_MAX), "012345");

  T (a7 + UR (1, 2), "012345");   /* { dg-warning "writing 7 bytes into a region of size 6" } */
  T (a7 + UR (2, 3), "012345");   /* { dg-warning "writing 7 bytes into a region of size 5" } */
  T (a7 + UR (6, 9), "012345");   /* { dg-warning "writing 7 bytes into a region of size 1" } */
  T (a7 + UR (7, 9), "012345");   /* { dg-warning "writing 7 bytes into a region of size 0" } */
  T (a7 + UR (8, 9), "012345");   /* { dg-warning "writing 7 bytes into a region of size 0" } */

  T (a7 + UR (9, 10), "012345");  /* { dg-warning "writing 7 bytes into a region of size 0" } */
  T (a7 + UR (DIFF_MAX, DIFF_MAX + (size_t)1), "012345");  /* { dg-warning "writing 7 bytes into a region of size 0" } */
  T (a7 + UR (DIFF_MAX, SIZE_MAX), "012345");  /* { dg-warning "writing 7 bytes into a region of size 0" } */

  char *d = a7 + 7;

  T (d + UR (-8, -7), "012345");
}

void test_strncpy_memarray (struct MemArray *p, const void *s)
{
#undef T
#define T(d, s, n) (strncpy ((d), (s), (n)), sink (d))

  T (p->a9 + UR (0, 1), s, 9);
  T (p->a9 + UR (0, 7), s, 9);
  T (p->a9 + UR (0, 8), s, 9);
  T (p->a9 + UR (0, DIFF_MAX), s, 9);
  T (p->a9 + UR (0, SIZE_MAX), s, 9);

  T (p->a9 + UR (1, 2), s, 9);    /* { dg-warning "writing 9 bytes into a region of size 8" } */
  T (p->a9 + UR (2, 3), s, 9);    /* { dg-warning "writing 9 bytes into a region of size 7" } */
  T (p->a9 + UR (6, 9), s, 9);    /* { dg-warning "writing 9 bytes into a region of size 3" } */
  T (p->a9 + UR (9, 10), s, 9);   /* { dg-warning "writing 9 bytes into a region of size 0" } */
  T (p->a9 + UR (10, 11), s, 9);  /* { dg-warning "writing 9 bytes into a region of size 0" } */

  T (p->a9 + UR (DIFF_MAX, DIFF_MAX + (size_t)1), s, 1);  /* { dg-warning "writing 1 byte into a region of size 0" } */
  T (p->a9 + UR (DIFF_MAX, SIZE_MAX), s, 3);  /* { dg-warning "writing 3 bytes into a region of size 0" } */
}
