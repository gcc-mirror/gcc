/* Test exercising -Wrawmem-overflow and -Wstringop-overflow warnings.  */
/* { dg-do compile } */
/* { dg-options "-O2 -Wstringop-overflow=2" } */

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


void test_memop_warn_object (const void *src)
{
  unsigned n = range (17, 29);

  struct A a[2];

  /* At both -Wstringop-overflow=2, like at 1, the destination of functions
     that operate on raw memory is considered to be the whole array and its
     size is therefore sizeof a.  */
  memcpy (&a[0], src, n);   /* { dg-warning "writing between 17 and 29 bytes into a region of size 4 overflows the destination" } */
  escape (a);
}

void test_memop_warn_subobject (const void *src)
{
  unsigned n = range (17, 31);

  struct B b[2];

  /* At -Wrawmem-overflow=2 the destination is considered to be
     the member sobobject of the first array element and its size
     is therefore sizeof b[0].a.  */
  memcpy (&b[0].a, src, n);   /* { dg-warning "writing between 17 and 31 bytes into a region of size 8 overflows the destination" } */

  escape (b);
}

void test_memop_nowarn_subobject (void)
{
  struct B b[2];

  /* The following idiom of clearing multiple members of a struct
     has been seen in a few places in the Linux kernel.  Verify
     that a warning is not issued for it.  */
  memset (&b[0].c, 0, sizeof b[0] - offsetof (struct B, c));

  escape (b);
}

struct C { char a[3], b; };
struct D { struct C c; char d, e; };

extern char* strncpy (char*, const char*, __SIZE_TYPE__);

void test_stringop_warn_object (const char *str)
{
  unsigned n = range (2 * sizeof (struct D), 32);

  struct C c[2];

  /* Similarly, at -Wstringop-overflow=2 the destination is considered
     to be the array member of the first element of the array c and its
     size is therefore sizeof c[0].a.  */
  strncpy (c[0].a, "123", n);   /* { dg-warning "writing between 12 and 32 bytes into a region of size 3 overflows the destination" } */
  escape (c);

  strncpy (c[0].a, str, n);   /* { dg-warning "writing between 12 and 32 bytes into a region of size 3 overflows the destination" } */
  escape (c);
}

void test_stringop_warn_subobject (const char *src)
{
  unsigned n = range (2 * sizeof (struct D), 32);

  struct D d[2];

  /* Same as above.  */
  strncpy (d[0].c.a, "123", n);   /* { dg-warning "writing between 12 and 32 bytes into a region of size 3 overflows the destination" } */
  escape (d);

  strncpy (d[0].c.a, src, n);   /* { dg-warning "writing between 12 and 32 bytes into a region of size 3 overflows the destination" } */
  escape (d);
}
