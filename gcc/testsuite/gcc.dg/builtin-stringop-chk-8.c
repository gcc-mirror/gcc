/* Test exercising -Wstringop-overflow warnings for reading past the end.  */
/* { dg-do compile } */
/* { dg-options "-O2 -Wstringop-overflow=1 -ftrack-macro-expansion=0" } */

#define PTRDIFF_MAX   __PTRDIFF_MAX__
#define SIZE_MAX      __SIZE_MAX__

#define offsetof(type, mem)   __builtin_offsetof (type, mem)

/* Return the number of bytes from member MEM of TYPE to the end
   of object OBJ.  */
#define offsetfrom(type, obj, mem) (sizeof (obj) - offsetof (type, mem))


typedef __SIZE_TYPE__ size_t;
extern void* memchr (const void*, int, size_t);
extern int memcmp (const void*, const void*, size_t);
extern void* memcpy (void*, const void*, size_t);
extern void* memmove (void*, const void*, size_t);
extern void* mempcpy (void*, const void*, size_t);

#define memchr(d, s, n) sink (memchr (d, s, n))
#define memcmp(d, s, n) sink (d, memcmp (d, s, n))
#define memcpy(d, s, n) sink (memcpy (d, s, n))
#define memmove(d, s, n) sink (memmove (d, s, n))
#define mempcpy(d, s, n) sink (mempcpy (d, s, n))

struct A { char a, b; };
struct B { struct A a; char c, d; };

/* Function to call to "escape" pointers from tests below to prevent
   GCC from assuming the values of the objects they point to stay
   the unchanged.  */
void sink (void*, ...);

/* Function to "generate" a random number each time it's called.  Declared
   (but not defined) and used to prevent GCC from making assumptions about
   their values based on the variables uses in the tested expressions.  */
size_t random_unsigned_value (void);

/* Return a random unsigned value between MIN and MAX.  */

static inline size_t
range (size_t min, size_t max)
{
  const size_t val = random_unsigned_value ();
  return val < min || max < val ? min : val;
}

#define R(min, max)   range (min, max)

/* Verify that reading beyond the end of a local array is diagnosed.  */

void test_memop_warn_local (void *p, const void *q)
{
  memcpy (p, "1234", R (6, 7));   /* { dg-warning "reading between 6 and 7 bytes from a region of size 5" } */

  struct A a[2];

  memcpy (p, a, R (7, 8));   /* { dg-warning "reading between 7 and 8 bytes from a region of size 4" } */

  /* At -Wstringop-overflow=1 the destination is considered to be
     the whole array and its size is therefore sizeof a.  */
  memcpy (p, &a[0], R (8, 9));   /* { dg-warning "reading between 8 and 9 bytes from a region of size 4" } */

  /* Verify the same as above but by reading from the first mmeber
     of the first element of the array.  */
  memcpy (p, &a[0].a, R (8, 9));   /* { dg-warning "reading between 8 and 9 bytes from a region of size 4" } */

  struct B b[2];

  memcpy (p, &b[0], R (12, 32));   /* { dg-warning "reading between 12 and 32 bytes from a region of size 8" } */

  /* Verify memchr/memcmp.  */
  int i = R (0, 255);
  memchr ("", i, 2);   /* { dg-warning "reading 2 bytes from a region of size 1" } */
  memchr ("", i, 2);   /* { dg-warning "reading 2 bytes from a region of size 1" } */
  memchr ("123", i, 5);   /* { dg-warning "reading 5 bytes from a region of size 4" } */
  memchr (a, i, sizeof a + 1);   /* { dg-warning "reading 5 bytes from a region of size 4" } */

  memcmp (p, "", 2);   /* { dg-warning "reading 2 bytes from a region of size 1" } */
  memcmp (p, "123", 5);   /* { dg-warning "reading 5 bytes from a region of size 4" } */
  memcmp (p, a, sizeof a + 1);   /* { dg-warning "reading 5 bytes from a region of size 4" } */

  size_t n = PTRDIFF_MAX + (size_t)1;
  memchr (p, 1, n);   /* { dg-warning "exceeds maximum object size" } */
  memcmp (p, q, n);   /* { dg-warning "exceeds maximum object size" } */

  n = SIZE_MAX;
  memchr (p, 1, n);   /* { dg-warning "exceeds maximum object size" } */
  memcmp (p, q, n);   /* { dg-warning "exceeds maximum object size" } */
}

/* Verify that reading beyond the end of a dynamically allocated array
   of known size is diagnosed.  */

void test_memop_warn_alloc (void *p)
{
  size_t n;

  n = range (8, 32);

  struct A *a = __builtin_malloc (sizeof *a * 2);

  memcpy (p, a, n);   /* { dg-warning "reading between 8 and 32 bytes from a region of size 4" "memcpy from allocated" } */

  memcpy (p, &a[0], n);   /* { dg-warning "reading between 8 and 32 bytes from a region of size 4" "memcpy from allocated" } */

  memcpy (p, &a[0].a, n);   /* { dg-warning "reading between 8 and 32 bytes from a region of size " "memcpy from allocated" } */

  n = range (12, 32);

  struct B *b = __builtin_malloc (sizeof *b * 2);

  memcpy (p, &b[0], n);   /* { dg-warning "reading between 12 and 32 bytes from a region of size 8" "memcpy from allocated" } */

  /* Verify memchr/memcmp.  */
  n = sizeof *b * 2 + 1;

  memchr (b, 1, n);   /* { dg-warning "reading 9 bytes from a region of size 8" "memcmp from allocated" } */
  memcmp (p, b, n);   /* { dg-warning "reading 9 bytes from a region of size 8" "memcmp from allocated" } */
}


void test_memop_nowarn (void *p)
{
  struct B b[2];

  size_t n = range (sizeof b, 32);

  /* Verify that copying the whole array is not diagnosed regardless
     of whether the expression pointing to its beginning is obtained
     from the array itself or its first member(s).  */
  memcpy (p, b, n);

  memcpy (p, &b[0], n);

  memcpy (p, &b[0].a, n);

  memcpy (p, &b[0].a.a, n);

  /* Verify that memchr/memcmp doesn't cause a warning.  */
  memchr (p, 1, n);
  memchr (b, 2, n);
  memchr (&b[0], 3, n);
  memchr (&b[0].a, 4, n);
  memchr (&b[0].a.a, 5, n);
  memchr ("01234567", R (0, 255), n);

  memcmp (p, p, n);
  memcmp (p, b, n);
  memcmp (p, &b[0], n);
  memcmp (p, &b[0].a, n);
  memcmp (p, &b[0].a.a, n);
  memcmp (p, "01234567", n);
}


/* The following function could specify in its API that it takes
   an array of exactly two elements, as shown below (or simply be
   called with such an array).  Verify that reading from both
   elements is not diagnosed.  */
void test_memop_nowarn_arg (void*, const struct A[2]);

void test_memop_nowarn_arg (void *p, const struct A *a)
{
  memcpy (p, a, 2 * sizeof *a);

  memcpy (p, a, range (2 * sizeof *a, 123));

  memchr (p, 1, 1234);
  memcmp (p, a, 1234);
}
