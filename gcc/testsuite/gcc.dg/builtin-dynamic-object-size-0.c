/* { dg-do run } */
/* { dg-options "-O2" } */
/* { dg-require-effective-target size20plus } */
/* { dg-additional-options "-DSKIP_STRNDUP" { target { ! strndup } } } */

#include "builtin-object-size-common.h"

typedef __SIZE_TYPE__ size_t;
#define abort __builtin_abort

void *
__attribute__ ((alloc_size (1)))
__attribute__ ((__nothrow__ , __leaf__))
__attribute__ ((noinline))
alloc_func_long (long sz)
{
  return __builtin_malloc (sz);
}

void *
__attribute__ ((alloc_size (1)))
__attribute__ ((__nothrow__ , __leaf__))
__attribute__ ((noinline))
alloc_func (size_t sz)
{
  return __builtin_malloc (sz);
}

void *
__attribute__ ((alloc_size (1, 2)))
__attribute__ ((__nothrow__ , __leaf__))
__attribute__ ((noinline))
calloc_func (size_t cnt, size_t sz)
{
  return __builtin_calloc (cnt, sz);
}

void *
__attribute__ ((noinline))
unknown_allocator (size_t cnt, size_t sz)
{
  return __builtin_calloc (cnt, sz);
}

size_t
__attribute__ ((noinline))
test_unknown (size_t cnt, size_t sz)
{
  void *ch = unknown_allocator (cnt, sz);
  size_t ret = __builtin_dynamic_object_size (ch, 0);
  __builtin_free (ch);
  return ret;
}

/* Malloc-like allocator.  */

size_t
__attribute__ ((noinline))
test_malloc (size_t sz)
{
  void *ch = alloc_func (sz);
  size_t ret = __builtin_dynamic_object_size (ch, 0);
  __builtin_free (ch);
  return ret;
}

size_t
__attribute__ ((noinline))
test_builtin_malloc (size_t sz)
{
  void *ch = __builtin_malloc (sz);
  size_t ret = __builtin_dynamic_object_size (ch, 0);
  __builtin_free (ch);
  return ret;
}

size_t
__attribute__ ((noinline))
test_builtin_malloc_cond (int cond)
{
  void *ch = __builtin_malloc (cond ? 32 : 64);
  size_t ret = __builtin_dynamic_object_size (ch, 0);
  __builtin_free (ch);
  return ret;
}

size_t
__attribute__ ((noinline))
test_builtin_malloc_condphi (int cond)
{
  void *ch;
 
  if (cond)
    ch = __builtin_malloc (32);
  else
    ch = __builtin_malloc (64);

  size_t ret = __builtin_dynamic_object_size (ch, 0);

  __builtin_free (ch);
  return ret;
}

size_t
__attribute__ ((noinline))
test_builtin_malloc_condphi2 (int cond, size_t in)
{
  void *ch;

  if (cond)
    ch = __builtin_malloc (in);
  else
    ch = __builtin_malloc (64);

  size_t ret = __builtin_dynamic_object_size (ch, 0);

  __builtin_free (ch);
  return ret;
}

size_t
__attribute__ ((noinline))
test_builtin_malloc_condphi3 (int cond, size_t in, size_t in2)
{
  void *ch;

  if (cond)
    ch = __builtin_malloc (in);
  else
    ch = __builtin_malloc (in2);

  size_t ret = __builtin_dynamic_object_size (ch, 0);

  __builtin_free (ch);
  return ret;
}

size_t
__attribute__ ((noinline))
test_builtin_malloc_condphi4 (size_t sz, int cond)
{
  char *a = __builtin_malloc (sz);
  char b[sz / 2];

  size_t ret = __builtin_dynamic_object_size (cond ? b : (void *) &a, 0);
  __builtin_free (a);
  return ret;
}

size_t
__attribute__ ((noinline))
test_builtin_malloc_condphi5 (size_t sz, int cond, char *c)
{
  char *a = __builtin_malloc (sz);

  size_t ret = __builtin_dynamic_object_size (cond ? c : (void *) &a, 0);
  __builtin_free (a);
  return ret;
}

long
__attribute__ ((noinline))
test_builtin_malloc_long (long sz, long off)
{
  char *a = alloc_func_long (sz);
  char *dest = a + off;
  long ret = __builtin_dynamic_object_size (dest, 0);
  return ret;
}

/* Calloc-like allocator.  */

size_t
__attribute__ ((noinline))
test_calloc (size_t cnt, size_t sz)
{
  void *ch = calloc_func (cnt, sz);
  size_t ret = __builtin_dynamic_object_size (ch, 0);
  __builtin_free (ch);
  return ret;
}

size_t
__attribute__ ((noinline))
test_builtin_calloc (size_t cnt, size_t sz)
{
  void *ch = __builtin_calloc (cnt, sz);
  size_t ret = __builtin_dynamic_object_size (ch, 0);
  __builtin_free (ch);
  return ret;
}

size_t
__attribute__ ((noinline))
test_builtin_calloc_cond (int cond1, int cond2)
{
  void *ch = __builtin_calloc (cond1 ? 32 : 64, cond2 ? 1024 : 16);
  size_t ret = __builtin_dynamic_object_size (ch, 0);
  __builtin_free (ch);
  return ret;
}

size_t
__attribute__ ((noinline))
test_builtin_calloc_condphi (size_t cnt, size_t sz, int cond)
{
  struct
    {
      int a;
      char b;
    } bin[cnt];

  char *ch = __builtin_calloc (cnt, sz);
  size_t ret = __builtin_dynamic_object_size (cond ? ch : (void *) &bin, 0);

  __builtin_free (ch);
  return ret;
}

/* Passthrough functions.  */

size_t
__attribute__ ((noinline))
test_passthrough (size_t sz, char *in)
{
  char *bin = __builtin_malloc (sz);
  char *dest = __builtin_memcpy (bin, in, sz);

  size_t ret = __builtin_dynamic_object_size (dest, 0);
  __builtin_free (bin);
  return ret;
}

size_t
__attribute__ ((noinline))
test_passthrough_nonssa (char *in)
{
  char bin[__builtin_strlen (in) + 1];
  char *dest = __builtin_memcpy (bin, in, __builtin_strlen (in) + 1);

  return __builtin_dynamic_object_size (dest, 0);
}

/* Variable length arrays.  */
size_t
__attribute__ ((noinline))
test_dynarray (size_t sz)
{
  char bin[sz];

  return __builtin_dynamic_object_size (bin, 0);
}

size_t
__attribute__ ((noinline))
test_dynarray_cond (int cond)
{
  char bin[cond ? 8 : 16];

  return __builtin_dynamic_object_size (bin, 0);
}

size_t
__attribute__ ((noinline))
test_deploop (size_t sz, size_t cond)
{
  char *bin = __builtin_alloca (32);

  for (size_t i = 0; i < sz; i++)
    if (i == cond)
      bin = __builtin_alloca (sz);

  return __builtin_dynamic_object_size (bin, 0);
}

/* Address expressions.  */

struct dynarray_struct
{
  long a;
  char c[16];
  int b;
};

size_t
__attribute__ ((noinline))
test_dynarray_struct (size_t sz, size_t off)
{
  struct dynarray_struct bin[sz];

  return __builtin_dynamic_object_size (&bin[off].c, 0);
}

size_t
__attribute__ ((noinline))
test_dynarray_struct_subobj (size_t sz, size_t off)
{
  struct dynarray_struct bin[sz];

  return __builtin_dynamic_object_size (&bin[off].c[4], 1);
}

size_t
__attribute__ ((noinline))
test_dynarray_struct_subobj2 (size_t sz, size_t off, size_t *objsz)
{
  struct dynarray_struct2
    {
      long a;
      int b;
      char c[sz];
    };

  struct dynarray_struct2 bin;

  *objsz = sizeof (bin);

  return __builtin_dynamic_object_size (&bin.c[off], 1);
}

/* See pr #108522.  */

#define DEFSTRUCT(_s, _n) \
  struct DS								      \
    {									      \
      char a[_n];							      \
      unsigned long long b;						      \
      int c;								      \
      char d[2 * _n];							      \
    } _s

size_t
__attribute__ ((noinline))
test_dynarray_struct_member_b (size_t sz)
{
  DEFSTRUCT (s, sz);

  return __builtin_dynamic_object_size (&s.b, 0);
}

size_t
__attribute__ ((noinline))
test_dynarray_struct_member_c (size_t sz)
{
  DEFSTRUCT (s, sz);

  return __builtin_dynamic_object_size (&s.c, 0);
}

size_t
__attribute__ ((noinline))
test_dynarray_struct_member_d (size_t sz, size_t offset)
{
  DEFSTRUCT (s, sz);

  return __builtin_dynamic_object_size (&s.d[offset], 0);
}

size_t
__attribute__ ((noinline))
test_dynarray_struct_member_subobj_b (size_t sz)
{
  DEFSTRUCT (s, sz);

  return __builtin_dynamic_object_size (&s.b, 1);
}

size_t
__attribute__ ((noinline))
test_dynarray_struct_member_subobj_c (size_t sz)
{
  DEFSTRUCT (s, sz);

  return __builtin_dynamic_object_size (&s.c, 1);
}

size_t
__attribute__ ((noinline))
test_dynarray_struct_member_subobj_d (size_t sz, size_t offset)
{
  DEFSTRUCT (s, sz);

  return __builtin_dynamic_object_size (&s.d[offset], 1);
}

size_t
__attribute__ ((noinline))
test_substring (size_t sz, size_t off)
{
  char str[sz];

  return __builtin_dynamic_object_size (&str[off], 0);
}

struct S2
{
  char arr[7];
};

struct S1
{
  int pad;
  struct S2 s2;
};

static long
g (struct S1 *s1)
{
  struct S2 *s2 = &s1->s2;
  return __builtin_dynamic_object_size (s2->arr, 0);
}

long
__attribute__ ((noinline))
test_alloc_nested_structs (int x)
{
  struct S1 *s1 = __builtin_malloc (x);
  return g (s1);
}

/* POINTER_PLUS expressions.  */

size_t
__attribute__ ((noinline))
test_substring_ptrplus (size_t sz, size_t off)
{
  int str[sz];

  return __builtin_dynamic_object_size (str + off, 0);
}

size_t
__attribute__ ((noinline))
test_substring_ptrplus2 (size_t sz, size_t off, size_t off2)
{
  int str[sz];
  int *ptr = &str[off];

  return __builtin_dynamic_object_size (ptr + off2, 0);
}

/* Function parameters.  */

size_t
__attribute__ ((access (__read_write__, 1, 2)))
__attribute__ ((noinline))
test_parmsz_simple (void *obj, size_t sz)
{
  return __builtin_dynamic_object_size (obj, 0);
}

size_t
__attribute__ ((access (__read_write__, 2, 1)))
__attribute__ ((noinline))
test_parmsz_simple2 (size_t sz, char obj[])
{
  return __builtin_dynamic_object_size (obj, 0);
}

size_t
__attribute__ ((noinline))
test_parmsz_simple3 (size_t sz, char obj[sz])
{
  return __builtin_dynamic_object_size (obj, 0);
}

size_t
__attribute__ ((noinline))
__attribute__ ((access (__read_write__, 1, 2)))
test_parmsz (void *obj, size_t sz, size_t off)
{
  return __builtin_dynamic_object_size (obj + off, 0);
}

size_t
__attribute__ ((access (__read_write__, 1, 2)))
__attribute__ ((noinline))
test_parmsz_scaled (int *obj, size_t sz)
{
  return __builtin_dynamic_object_size (obj, 0);
}

size_t
__attribute__ ((noinline))
__attribute__ ((access (__read_write__, 1, 2)))
test_parmsz_scaled_off (int *obj, size_t sz, size_t off)
{
  return __builtin_dynamic_object_size (obj + off, 0);
}

size_t
__attribute__ ((access (__read_write__, 1, 3)))
__attribute__ ((noinline))
test_parmsz_unknown (void *obj, void *unknown, size_t sz, int cond)
{
  return __builtin_dynamic_object_size (cond ? obj : unknown, 0);
}

struct S;
size_t
__attribute__ ((access (__read_write__, 1, 2)))
__attribute__ ((noinline))
test_parmsz_extern (struct S *obj, size_t sz)
{
  return __builtin_dynamic_object_size (obj, 0);
}

/* Implicitly constructed access attributes not supported yet.  */
size_t
__attribute__ ((noinline))
test_parmsz_internal (size_t sz, double obj[][sz])
{
  return __builtin_dynamic_object_size (obj, 0);
}

size_t
__attribute__ ((access (__read_write__, 2, 1)))
__attribute__ ((noinline))
test_parmsz_internal2 (size_t sz, double obj[][sz])
{
  return __builtin_dynamic_object_size (obj, 0);
}

size_t
__attribute__ ((noinline))
test_parmsz_internal3 (size_t sz1, size_t sz2, double obj[sz1][sz2])
{
  return __builtin_dynamic_object_size (obj, 0);
}

size_t
__attribute__ ((noinline))
test_parmsz_internal4 (size_t sz1, size_t sz2, double obj[sz1 + 1][4])
{
  return __builtin_dynamic_object_size (obj, 0);
}

/* Loops.  */

size_t
__attribute__ ((noinline))
__attribute__ ((access (__read_write__, 1, 2)))
test_loop (int *obj, size_t sz, size_t start, size_t end, int incr)
{
  int *ptr = obj + start;

  for (int i = start; i != end; i = i + incr)
    {
      ptr = ptr + incr;
      if (__builtin_dynamic_object_size (ptr, 0) == 0)
	return 0;
    }

  return __builtin_dynamic_object_size (ptr, 0);
}

/* strdup/strndup.  */

size_t
__attribute__ ((noinline))
test_strdup (const char *in)
{
  char *res = __builtin_strdup (in);
  size_t sz = __builtin_dynamic_object_size (res, 0);

  __builtin_free (res);
  return sz;
}

#ifndef SKIP_STRNDUP
size_t
__attribute__ ((noinline))
test_strndup (const char *in, size_t bound)
{
  char *res = __builtin_strndup (in, bound);
  size_t sz = __builtin_dynamic_object_size (res, 0);

  __builtin_free (res);
  return sz;
}
#endif

size_t
__attribute__ ((noinline))
test_strdup_min (const char *in)
{
  char *res = __builtin_strdup (in);
  size_t sz = __builtin_dynamic_object_size (res, 2);

  __builtin_free (res);
  return sz;
}

#ifndef SKIP_STRNDUP
size_t
__attribute__ ((noinline))
test_strndup_min (const char *in, size_t bound)
{
  char *res = __builtin_strndup (in, bound);
  size_t sz = __builtin_dynamic_object_size (res, 2);

  __builtin_free (res);
  return sz;
}
#endif

/* Other tests.  */

struct TV4
{
  __attribute__((vector_size (sizeof (int) * 4))) int v;
};

struct TV4 val3;
int *
test_pr105736 (struct TV4 *a)
{
  return &a->v[0];
}

int
main (int argc, char **argv)
{
  size_t outsz = test_unknown (32, 42);
  if (outsz != -1 && outsz != 32)
    FAIL ();
  if (test_malloc (2048) != 2048)
    FAIL ();
  if (test_builtin_malloc (2048) != 2048)
    FAIL ();
  if (test_builtin_malloc_cond (1) != 32)
    FAIL ();
  if (test_builtin_malloc_cond (0) != 64)
    FAIL ();
  if (test_builtin_malloc_condphi (1) != 32)
    FAIL ();
  if (test_builtin_malloc_condphi (0) != 64)
    FAIL ();
  if (test_builtin_malloc_condphi2 (1, 128) != 128)
    FAIL ();
  if (test_builtin_malloc_condphi2 (0, 128) != 64)
    FAIL ();
  if (test_builtin_malloc_condphi3 (1, 128, 256) != 128)
    FAIL ();
  if (test_builtin_malloc_condphi3 (0, 128, 256) != 256)
    FAIL ();
  if (test_builtin_malloc_condphi4 (128, 1) != 64)
    FAIL ();
  if (test_builtin_malloc_condphi4 (128, 0) != sizeof (void *))
    FAIL ();
  if (test_builtin_malloc_condphi5 (128, 0, argv[0]) != -1)
    FAIL ();
  long x = 42;
  if (test_builtin_malloc_long (x, 0) != x)
    FAIL ();
  if (test_calloc (2048, 4) != 2048 * 4)
    FAIL ();
  if (test_builtin_calloc (2048, 8) != 2048 * 8)
    FAIL ();
  if (test_builtin_calloc_cond (0, 0) != 64 * 16)
    FAIL ();
  if (test_builtin_calloc_cond (1, 1) != 32 * 1024)
    FAIL ();
  if (test_builtin_calloc_condphi (128, 1, 0)
      != 128 * sizeof (struct { int a; char b; }))
    FAIL ();
  if (test_builtin_calloc_condphi (128, 1, 1) != 128)
    FAIL ();
  if (test_passthrough (__builtin_strlen (argv[0]) + 1, argv[0])
      != __builtin_strlen (argv[0]) + 1)
    FAIL ();
  if (test_passthrough_nonssa (argv[0]) != __builtin_strlen (argv[0]) + 1)
    FAIL ();
  if (test_dynarray (__builtin_strlen (argv[0])) != __builtin_strlen (argv[0]))
    FAIL ();
  if (test_dynarray_struct (42, 4) !=
      ((42 - 4) * sizeof (struct dynarray_struct)
       - __builtin_offsetof (struct dynarray_struct, c)))
    FAIL ();
  if (test_dynarray_struct (42, 48) != 0)
    FAIL ();
  if (test_substring (128, 4) != 128 - 4)
    FAIL ();
  if (test_substring (128, 142) != 0)
    FAIL ();
  if (test_dynarray_struct_subobj (42, 4) != 16 - 4)
    FAIL ();
  if (test_dynarray_struct_subobj (42, 48) != 0)
    FAIL ();
  size_t objsz = 0;
  if (test_dynarray_struct_subobj2 (42, 4, &objsz)
    != objsz - 4 - sizeof (long) - sizeof (int))
    FAIL ();
  DEFSTRUCT(ds, 64);
  const size_t n = sizeof (ds.a);
  if (test_dynarray_struct_member_b (n)
      != sizeof (ds) - __builtin_offsetof (struct DS, b))
    FAIL ();
  if (test_dynarray_struct_member_c (n)
      != sizeof (ds) - __builtin_offsetof (struct DS, c))
    FAIL ();
  if (test_dynarray_struct_member_d (n, 0)
      != sizeof (ds) - __builtin_offsetof (struct DS, d))
    FAIL ();
  if (test_dynarray_struct_member_subobj_b (n) != sizeof (ds.b))
    FAIL ();
  if (test_dynarray_struct_member_subobj_c (n) != sizeof (ds.c))
    FAIL ();
  if (test_dynarray_struct_member_subobj_d (n, n - 2)
      != sizeof (ds) - __builtin_offsetof (struct DS, d) - n + 2)
    FAIL ();
  if (test_substring_ptrplus (128, 4) != (128 - 4) * sizeof (int))
    FAIL ();
  if (test_substring_ptrplus (128, 142) != 0)
    FAIL ();
  if (test_substring_ptrplus2 (128, 4, 4) != (128 - 8) * sizeof (int))
    FAIL ();
  if (test_substring_ptrplus2 (128, 4, -3) != (128 - 1) * sizeof (int))
    FAIL ();
  if (test_dynarray_cond (0) != 16)
    FAIL ();
  if (test_dynarray_cond (1) != 8)
    FAIL ();
  if (test_alloc_nested_structs (42) != 42 - sizeof (int))
    FAIL ();
  if (test_deploop (128, 4) != 128)
    FAIL ();
  if (test_deploop (128, 129) != 32)
    FAIL ();
  if (test_parmsz_simple (argv[0], __builtin_strlen (argv[0]) + 1)
      != __builtin_strlen (argv[0]) + 1)
    FAIL ();
  if (test_parmsz_simple2 (__builtin_strlen (argv[0]) + 1, argv[0])
      != __builtin_strlen (argv[0]) + 1)
    FAIL ();
  if (test_parmsz_simple3 (__builtin_strlen (argv[0]) + 1, argv[0]) 
      != __builtin_strlen (argv[0]) + 1)
    FAIL ();
  int arr[42];
  if (test_parmsz_scaled (arr, 42) != sizeof (arr))
    FAIL ();
  if (test_parmsz_scaled (arr, 40) != 40 * sizeof (int))
    FAIL ();
  /* __bdos cannot see the actual size of ARR, so it will return what it was
     passed.  Fortunately though the overflow warnings see this caller side and
     warns of the problematic size.  */
  if (test_parmsz_scaled (arr, 44) != 44 * sizeof (int)) /* { dg-warning "-Wstringop-overflow=" } */
    FAIL ();
  if (test_parmsz_unknown (argv[0], argv[0], __builtin_strlen (argv[0]) + 1, 0)
      != -1)
  if (test_parmsz (argv[0], __builtin_strlen (argv[0]) + 1, -1) != 0)
    FAIL ();
  if (test_parmsz (argv[0], __builtin_strlen (argv[0]) + 1, 0)
      != __builtin_strlen (argv[0]) + 1)
    FAIL ();
  if (test_parmsz (argv[0], __builtin_strlen (argv[0]) + 1,
		   __builtin_strlen (argv[0])) != 1)
    FAIL ();
  if (test_parmsz (argv[0], __builtin_strlen (argv[0]) + 1,
		   __builtin_strlen (argv[0]) + 2) != 0)
    FAIL ();
  if (test_parmsz_scaled_off (arr, 42, 2) != 40 * sizeof (int))
    FAIL ();
  struct S *s;
  if (test_parmsz_extern (s, 42) != -1)
    FAIL ();
  double obj[4][4];
  if (test_parmsz_internal (4, obj) != -1)
    FAIL ();
  if (test_parmsz_internal2 (4, obj) != -1)
    FAIL ();
  if (test_parmsz_internal3 (4, 4, obj) != -1)
    FAIL ();
  if (test_parmsz_internal4 (3, 4, obj) != -1)
    FAIL ();
  if (test_loop (arr, 42, 0, 32, 1) != 10 * sizeof (int))
    FAIL ();
  if (test_loop (arr, 42, 32, -1, -1) != 0)
    FAIL ();
  if (test_loop (arr, 42, 32, 10, -1) != 32 * sizeof (int))
    FAIL ();
  if (test_loop (arr, 42, 42, 0, -1) != 42 * sizeof (int))
    FAIL ();
  if (test_loop (arr, 42, 44, 0, -1) != 0)
    FAIL ();
  if (test_loop (arr, 42, 20, 52, 1) != 0)
    FAIL ();
  /* pr105736.  */
  int *t = test_pr105736 (&val3);
  if (__builtin_dynamic_object_size (t, 0) != -1)
    FAIL ();
  const char *str = "hello world";
  if (test_strdup (str) != __builtin_strlen (str) + 1)
    FAIL ();
#ifndef SKIP_STRNDUP
  if (test_strndup (str, 4) != 5)
    FAIL ();
#endif
  if (test_strdup_min (str) != __builtin_strlen (str) + 1)
    FAIL ();
#ifndef SKIP_STRNDUP
  if (test_strndup_min (str, 4) != 1)
    FAIL ();
#endif

  DONE ();
}
