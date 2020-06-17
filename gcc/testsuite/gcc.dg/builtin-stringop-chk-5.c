/* Test exercising -Wstringop-overflow warnings.  */
/* { dg-do compile } */
/* { dg-options "-O2 -Wstringop-overflow=1 -Wno-array-bounds" } */

#define offsetof(type, mem)   __builtin_offsetof (type, mem)

/* Return the number of bytes from member MEM of TYPE to the end
   of object OBJ.  */
#define offsetfrom(type, obj, mem) (sizeof (obj) - offsetof (type, mem))


typedef __SIZE_TYPE__ size_t;
extern void* memcpy (void*, const void*, size_t);
extern void* memset (void*, int, __SIZE_TYPE__);


struct A { char a, b; };
struct B { struct A a; char c, d; };

/* Function to call to "escape" pointers from tests below to prevent
   GCC from assuming the values of the objects they point to stay
   the unchanged.  */
void escape (void*, ...);

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

/* Verify that writing past the end of a local array is diagnosed.  */

void test_memop_warn_local (const void *src)
{
  size_t n;

  n = range (8, 32);

  struct A a[2];

  memcpy (a, src, n);   /* { dg-warning "writing between 8 and 32 bytes into a region of size 4 overflows the destination" } */
  escape (a, src);

  /* At -Wstringop-overflow=1 the destination is considered to be
     the whole array and its size is therefore sizeof a.  */
  memcpy (&a[0], src, n);   /* { dg-warning "writing between 8 and 32 bytes into a region of size 4 overflows the destination" } */
  escape (a, src);

  /* Verify the same as above but by writing into the first mmeber
     of the first element of the array.  */
  memcpy (&a[0].a, src, n);   /* { dg-warning "writing between 8 and 32 bytes into a region of size 4 overflows the destination" } */
  escape (a, src);

  n = range (12, 32);

  struct B b[2];

  memcpy (&b[0], src, n);   /* { dg-warning "writing between 12 and 32 bytes into a region of size 8 overflows the destination" } */
  escape (b);

  /* The following idiom of clearing multiple members of a struct is
     used in a few places in the Linux kernel.  Verify that a warning
     is issued for it when it writes past the end of the array object.  */
  memset (&b[0].a.b, 0, offsetfrom (struct B, b, a.b) + 1);   /* { dg-warning "writing 8 bytes into a region of size 7" } */
  escape (b);

  memset (&b->a.b, 0, offsetfrom (struct B, b, a.b) + 1);   /* { dg-warning "writing 8 bytes into a region of size 7" } */
  escape (b);

  memset (&b[0].c, 0, offsetfrom (struct B, b, c) + 1);   /* { dg-warning "writing 7 bytes into a region of size 6" } */
  escape (b);

  memset (&b->c, 0, offsetfrom (struct B, b, c) + 1);   /* { dg-warning "writing 7 bytes into a region of size 6" } */
  escape (b);

  memset (&b[0].d, 0, offsetfrom (struct B, b, d) + 1);   /* { dg-warning "writing 6 bytes into a region of size 5" } */
  escape (b);

  memset (&b->d, 0, offsetfrom (struct B, b, d) + 1);   /* { dg-warning "writing 6 bytes into a region of size 5" } */
  escape (b);

  /* Same as above but clearing just elements of the second element
     of the array.  */
  memset (&b[1].a.b, 0, offsetfrom (struct B, b[1], a.b) + 1);   /* { dg-warning "writing 4 bytes into a region of size 3" } */
  escape (b);

  memset (&b[1].c, 0, offsetfrom (struct B, b[1], c) + 1);   /* { dg-warning "writing 3 bytes into a region of size 2" } */
  escape (b);

  memset (&b[1].d, 0, offsetfrom (struct B, b[1], d) + 1);   /* { dg-warning "writing 2 bytes into a region of size 1" } */
  escape (b);
}

/* Verify that writing past the end of a dynamically allocated array
   of known size is diagnosed.  */

void test_memop_warn_alloc (const void *src)
{
  size_t n;

  n = range (8, 32);

  struct A *a = __builtin_malloc (sizeof *a * 2);

  memcpy (a, src, n);   /* { dg-warning "writing between 8 and 32 bytes into a region of size 4 " "memcpy into allocated" } */
  escape (a, src);

  /* At -Wstringop-overflow=1 the destination is considered to be
     the whole array and its size is therefore sizeof a.  */
  memcpy (&a[0], src, n);   /* { dg-warning "writing between 8 and 32 bytes into a region of size 4 overflows the destination" "memcpy into allocated" } */
  escape (a, src);

  /* Verify the same as above but by writing into the first mmeber
     of the first element of the array.  */
  memcpy (&a[0].a, src, n);   /* { dg-warning "writing between 8 and 32 bytes into a region of size " "memcpy into allocated" } */
  escape (a, src);

  n = range (12, 32);

  struct B *b = __builtin_malloc (sizeof (struct B[2]));

  memcpy (&b[0], src, n);   /* { dg-warning "writing between 12 and 32 bytes into a region of size 8 " "memcpy into allocated" } */
  escape (b);

  /* The following idiom of clearing multiple members of a struct is
     used in a few places in the Linux kernel.  Verify that a warning
     is issued for it when it writes past the end of the array object.  */
  memset (&b[0].a.b, 0, offsetfrom (struct B, struct B[2], a.b) + 1);   /* { dg-warning "writing 8 bytes into a region of size " "memcpy into allocated" } */
  escape (b);

  memset (&b->a.b, 0, offsetfrom (struct B, struct B[2], a.b) + 1);   /* { dg-warning "writing 8 bytes into a region of size " "memcpy into allocated" } */
  escape (b);

  memset (&b[0].c, 0, offsetfrom (struct B, struct B[2], c) + 1);   /* { dg-warning "writing 7 bytes into a region of size " "memcpy into allocated" } */
  escape (b);

  memset (&b->c, 0, offsetfrom (struct B, struct B[2], c) + 1);   /* { dg-warning "writing 7 bytes into a region of size " "memcpy into allocated" } */
  escape (b);

  memset (&b[0].d, 0, offsetfrom (struct B, struct B[2], d) + 1);   /* { dg-warning "writing 6 bytes into a region of size " "memcpy into allocated" } */
  escape (b);

  memset (&b->d, 0, offsetfrom (struct B, struct B[2], d) + 1);   /* { dg-warning "writing 6 bytes into a region of size " "memcpy into allocated" } */
  escape (b);

  /* Same as above but clearing just elements of the second element
     of the array.  */
  memset (&b[1].a.b, 0, offsetfrom (struct B, b[1], a.b) + 1);   /* { dg-warning "writing 4 bytes into a region of size " "memcpy into allocated" } */
  escape (b);

  memset (&b[1].c, 0, offsetfrom (struct B, b[1], c) + 1);   /* { dg-warning "writing 3 bytes into a region of size " "memcpy into allocated" } */
  escape (b);

  memset (&b[1].d, 0, offsetfrom (struct B, b[1], d) + 1);   /* { dg-warning "writing 2 bytes into a region of size 1" "memcpy into allocated" } */
  escape (b);
}


void test_memop_nowarn (const void *src)
{
  struct B b[2];

  size_t n = range (sizeof b, 32);

  /* Verify that clearing the whole array is not diagnosed regardless
     of whether the expression pointing to its beginning is obtained
     from the array itself or its first member(s).  */
  memcpy (b, src, n);
  escape (b);

  memcpy (&b[0], src, n);
  escape (b);

  memcpy (&b[0].a, src, n);
  escape (b, src);

  memcpy (&b[0].a.a, src, n);
  escape (b, src);

  /* Clearing multiple elements of an array of structs.  */
  memset (&b[0].a.b, 0, sizeof b - offsetof (struct B, a.b));
  escape (b);

  memset (&b->a.b, 0, sizeof b - offsetof (struct B, a.b));
  escape (b);

  memset (&b[0].c, 0, sizeof b - offsetof (struct B, c));
  escape (b);

  memset (&b->c, 0, sizeof b - offsetof (struct B, c));
  escape (b);

  memset (&b[0].d, 0, sizeof b - offsetof (struct B, d));
  escape (b);

  memset (&b->d, 0, sizeof b - offsetof (struct B, d));
  escape (b);

  /* Same as above but clearing just elements of the second element
     of the array.  */
  memset (&b[1].a.b, 0, sizeof b[1] - offsetof (struct B, a.b));
  escape (b);

  memset (&b[1].c, 0, sizeof b[1] - offsetof (struct B, c));
  escape (b);

  memset (&b[1].d, 0, sizeof b[1] - offsetof (struct B, d));
  escape (b);
}


/* The foollowing function could specify in its API that it takes
   an array of exactly two elements, as shown below.  Verify that
   writing into both elements is not diagnosed.  */
void test_memop_nowarn_arg (struct A[2], const void*);

void test_memop_nowarn_arg (struct A *a, const void *src)
{
  memcpy (a, src, 2 * sizeof *a);
  escape (a, src);

  memcpy (a, src, range (2 * sizeof *a, 123));
  escape (a, src);
}


struct C { char a[3], b; };
struct D { struct C c; char d, e; };

extern char* strncpy (char*, const char*, __SIZE_TYPE__);

void test_stringop_warn (void)
{
  size_t n = range (2 * sizeof (struct D) + 1, 33);

  struct C c[2];

  /* Similarly, at -Wstringop-overflow=1 the destination is considered
     to be the whole array and its size is therefore sizeof c.  */
  strncpy (c[0].a, "123", n);   /* { dg-warning "writing between 13 and 33 bytes into a region of size 8 overflows the destination" } */

  escape (c);
}


void test_stringop_nowarn (void)
{
  struct D d[2];

  strncpy (d[0].c.a, "123", range (sizeof d, 32));
  escape (d);
}
