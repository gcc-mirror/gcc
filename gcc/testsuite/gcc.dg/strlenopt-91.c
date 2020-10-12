/* PR tree-optimization/92412 - excessive errno aliasing assumption defeats
   optimization
   { dg-do compile }
   { dg-options "-O2 -Wall -fdump-tree-optimized" }
   { dg-require-effective-target alloca } */

typedef __SIZE_TYPE__ size_t;

extern void* alloca (size_t);
extern void* calloc (size_t, size_t);
extern void* malloc (size_t);

extern const char exta[4];
static char stata[] = "123";

void sink (const void*, ...);

#define T(ptr, alloc) do {						\
    const char *p = ptr;						\
    if (p[0] != '1' || p[1] != '2' || p[2] != '3' || p[3] != '\0'	\
	|| __builtin_strlen (p) != 3)					\
      return;								\
									\
    void *q = alloc;							\
    __builtin_strcpy (q, p);						\
									\
    if (p[0] != '1' || p[1] != '2' || p[2] != '3' || p[3] != '\0'	\
	|| __builtin_strlen (p) != 3					\
	|| __builtin_strlen (q) != 3)					\
      __builtin_abort ();						\
									\
    sink (p, q);							\
  } while (0)


void alloca_test_local (unsigned n)
{
  char loca[] = "123";
  T (loca, alloca (n));
}

void alloca_test_extern_const (unsigned n)
{
  T (exta, alloca (n));
}

void alloca_test_static (unsigned n)
{
  T (stata, alloca (n));
}


// Verify fix for PR tree-optimization/92412.
void calloc_test_local (unsigned m, unsigned n)
{
  char loca[] = "123";
  T (loca, calloc (m, n));
}

void calloc_test_extern_const (unsigned m, unsigned n)
{
  T (exta, calloc (m, n));
}

void calloc_test_static (unsigned m, unsigned n)
{
  T (stata, calloc (m, n));
}


// Verify fix for PR tree-optimization/92412.
void malloc_test_local (unsigned n)
{
  char loca[] = "123";
  T (loca, malloc (n));
}

void malloc_test_extern_const (unsigned n)
{
  T (exta, malloc (n));
}

void malloc_test_static (unsigned n)
{
  T (stata, malloc (n));
}


#undef T
#define T(ptr, n) do {							\
    const char *p = ptr;						\
    if (p[0] != '1' || p[1] != '2' || p[2] != '3' || p[3] != '\0'	\
	|| __builtin_strlen (p) != 3)					\
      return;								\
									\
    char vla[n];							\
    char *q = vla;							\
    __builtin_strcpy (q, p);						\
									\
    if (p[0] != '1' || p[1] != '2' || p[2] != '3' || p[3] != '\0'	\
	|| __builtin_strlen (p) != 3					\
	|| __builtin_strlen (q) != 3)					\
      __builtin_abort ();						\
									\
    sink (p, vla);							\
  } while (0)


void vla_test_local (unsigned n)
{
  char loca[] = "123";
  T (loca, n);
}

void vla_test_extern_const (unsigned n)
{
  T (exta, n);
}

void vla_test_static (unsigned n)
{
  T (stata, n);
}

/* { dg-final { scan-tree-dump-not "abort" "optimized" } } */
