/* Test to exercise that -Warray-bounds warnings for memory and string
   functions are issued even when they are declared in system headers
   (i.e., not just when they are explicitly declared in the source
   file.)
   Also verify that the warnings are issued even for calls where the
   source of the excessive array bound is in a different function than
   the call.
   { dg-do compile }
   { dg-options "-O2 -Warray-bounds -Wno-stringop-overflow -fno-tree-vrp" } */

#if __has_include (<stddef.h>)
#  include <stddef.h>
#else
/* For cross-compilers.  */
typedef __PTRDIFF_TYPE__   ptrdiff_t;
typedef __SIZE_TYPE__      size_t;
#endif

#if __has_include (<string.h>)
#  include <string.h>
#  undef memcpy
#  undef strcat
#  undef strcpy
#  undef strncpy
#else
extern void* memcpy (void*, const void*, size_t);
extern char* strcat (char*, const char*);
extern char* strcpy (char*, const char*);
extern char* strncpy (char*, const char*, size_t);
#endif


#define MAX  (__SIZE_MAX__ / 2)

void sink (void*);

struct __attribute__ ((packed)) Array
{
  char a13[13];
  char a15[15];
  char a17[17];
};

/* Exercise memcpy out-of-bounds offsets with an array of known size.  */

static void
wrap_memcpy_src_xsize (char *d, const char *s, ptrdiff_t i, size_t n)
{
  memcpy (d, s + i, n);   /* { dg-warning "offset 46 is out of the bounds \\\[0, 45] of object .ar. with type .(struct )?Array." "memcpy" } */
}

void call_memcpy_src_xsize (char *d, size_t n)
{
  struct Array ar;
  sink (&ar);
  wrap_memcpy_src_xsize (d, ar.a13, 46, n);
  sink (&ar);
}

/* Exercise memcpy out-of-bounds offsets with an array of unknown size.  */

static void
wrap_memcpy_src_diff_max (char *d, const char *s, ptrdiff_t i, size_t n)
{
  memcpy (d, s + i, n);   /* { dg-warning "pointer overflow between offset \[0-9\]+ and size 3" "memcpy" } */
}

void call_memcpy_src_diff_max (char *d, const char *s, size_t n)
{
  wrap_memcpy_src_diff_max (d, s, MAX, 3);
}

static void
wrap_memcpy_dst_xsize (char *d, const char *s, ptrdiff_t i, size_t n)
{
  memcpy (d + i, s, n);   /* { dg-warning "offset 47 is out of the bounds \\\[0, 45] of object .ar1. with type .(struct )?Array." "memcpy" } */
}

void call_memcpy_dst_xsize (const char *s, size_t n)
{
  struct Array ar1;       /* { dg-message ".ar1. declared here" } */
  sink (&ar1);
  wrap_memcpy_dst_xsize (ar1.a15, s, 34, n);
  sink (&ar1);
}

static void
wrap_memcpy_dst_diff_max (char *d, const char *s, ptrdiff_t i, size_t n)
{
  memcpy (d + i, s, n);   /* { dg-warning "offset -?\[0-9\]+ is out of the bounds \\\[0, 45] of object .ar2. with type .(struct )?Array." "memcpy" } */
}

void call_memcpy_dst_diff_max (const char *s, size_t n)
{
  struct Array ar2;       /* { dg-message ".ar2. declared here" } */
  sink (&ar2);
  wrap_memcpy_dst_diff_max (ar2.a15, s, MAX, n);
  sink (&ar2);
}


static void wrap_strcat_src_xsize (char *d, const char *s, ptrdiff_t i)
{
  strcat (d, s + i);   /* { dg-warning "offset 46 is out of the bounds \\\[0, 45] of object .ar3. with type .(struct )?Array." "strcat" } */
}

void call_strcat_src_xsize (char *d)
{
  struct Array ar3;       /* { dg-message ".ar3. declared here" } */
  sink (&ar3);
  wrap_strcat_src_xsize (d, ar3.a15, 15 + 17 + 1);
  sink (&ar3);
}

static void wrap_strcat_dst_xsize (char *d, const char *s, ptrdiff_t i)
{
  strcat (d + i, s);   /* { dg-warning "offset 47 is out of the bounds \\\[0, 45] of object .ar4. with type .(struct )?Array." "strcat" } */
}

void call_strcat_dst_xsize (const char *s)
{
  struct Array ar4;       /* { dg-message ".ar4. declared here" } */
  sink (&ar4);
  wrap_strcat_dst_xsize (ar4.a15, s, 15 + 17 + 2);
  sink (&ar4);
}


static void wrap_strcpy_src_xsize (char *d, const char *s, ptrdiff_t i)
{
  strcpy (d, s + i);   /* { dg-warning "offset 48 is out of the bounds \\\[0, 45] of object .ar5. with type .(struct )?Array." "strcpy" } */
}

void call_strcpy_src_xsize (char *d)
{
  struct Array ar5;       /* { dg-message ".ar5. declared here" } */
  sink (&ar5);
  wrap_strcpy_src_xsize (d, ar5.a15, 15 + 17 + 3);
  sink (&ar5);
}

static void wrap_strcpy_dst_xsize (char *d, const char *s, ptrdiff_t i)
{
  strcpy (d + i, s);   /* { dg-warning "offset 49 is out of the bounds \\\[0, 45] of object .ar6. with type .(struct )?Array." "strcpy" } */
}

void call_strcpy_dst_xsize (const char *s)
{
  struct Array ar6;       /* { dg-message ".ar6. declared here" } */
  sink (&ar6);
  wrap_strcpy_dst_xsize (ar6.a15, s, 15 + 17 + 4);
  sink (&ar6);
}


/* Exercise strncpy out-of-bounds offsets with an array of known size.  */

static void
wrap_strncpy_src_xsize (char *d, const char *s, ptrdiff_t i, size_t n)
{
  strncpy (d, s + i, n);   /* { dg-warning "offset 46 is out of the bounds \\\[0, 45] of object .ar7. with type '(struct )?Array." "strncpy" } */
}

void call_strncpy_src_xsize (char *d, size_t n)
{
  struct Array ar7;       /* { dg-message ".ar7. declared here" } */
  sink (&ar7);
  wrap_strncpy_src_xsize (d, ar7.a17, 17 + 1, n);
  sink (&ar7);
}

/* Exercise strncpy out-of-bounds offsets with an array of unknown size.  */

static void
wrap_strncpy_src_diff_max_m1 (char *d, const char *s, ptrdiff_t i, size_t n)
{
  /* Unlike in the similar call to memcpy(), there is no pointer
     overflow here because the size N is not added to the source
     offset MAX - 1 (only 1 is for the access to its first element,
     which is tested below).  */
  strncpy (d, s + i, n);
}

void call_strncpy_src_diff_max_m1 (char *d, const char *s, size_t n)
{
  wrap_strncpy_src_diff_max_m1 (d, s, MAX - 1, 3);
}

static void
wrap_strncpy_src_diff_max (char *d, const char *s, ptrdiff_t i, size_t n)
{
  strncpy (d, s + i, n);  /* { dg-warning "pointer overflow between offset \[0-9\]+ and size \\\[1, 0]" } */
}

void call_strncpy_src_diff_max (char *d, const char *s, size_t n)
{
  wrap_strncpy_src_diff_max (d, s, MAX, 3);
}

static void
wrap_strncpy_dst_xsize (char *d, const char *s, ptrdiff_t i, size_t n)
{
  strncpy (d + i, s, n);   /* { dg-warning "offset 47 is out of the bounds \\\[0, 45] of object .ar8. with type .(struct )?Array." "strncpy" } */
}

void call_strncpy_dst_xsize (const char *s, size_t n)
{
  struct Array ar8;       /* { dg-message ".ar8. declared here" } */
  sink (&ar8);
  wrap_strncpy_dst_xsize (ar8.a17, s, 17 + 2, n);
  sink (&ar8);
}

static void
wrap_strncpy_dst_diff_max (char *d, const char *s, ptrdiff_t i, size_t n)
{
  strncpy (d + i, s, n);   /* { dg-warning "offset -\[0-9\]+ is out of the bounds \\\[0, 45] of object .ar9. with type .(struct )?Array." "strncpy" } */
}

void call_strncpy_dst_diff_max (const char *s, size_t n)
{
  struct Array ar9;       /* { dg-message ".ar9. declared here" "strncpy" } */
  sink (&ar9);
  wrap_strncpy_dst_diff_max (ar9.a17, s, MAX, n);
  sink (&ar9);
}

static void
wrap_strncpy_dstarray_diff_neg (char *d, const char *s, ptrdiff_t i, size_t n)
{
  strncpy (d + i, s, n);   /* { dg-warning "offset -\[0-9\]+ is out of the bounds \\\[0, 90] of object .ar10. with type .(struct )?Array ?\\\[2]." "strncpy" } */
}

void call_strncpy_dstarray_diff_neg (const char *s, size_t n)
{
  struct Array ar10[2];    /* { dg-message ".ar10. declared here" } */
  sink (&ar10);

  int off = (char*)ar10[1].a17 - (char*)ar10 + 1;
  wrap_strncpy_dstarray_diff_neg (ar10[1].a17, s, -off, n);

  sink (&ar10);
}
