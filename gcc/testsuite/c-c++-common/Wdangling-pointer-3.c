/* PR middle-end/63272 - GCC should warn when using pointer to dead scoped
   variable within the same function
   Exercise conditional uses dangling pointers with optimization.
   { dg-do compile }
   { dg-options "-O2 -Wall -Wno-maybe-uninitialized" } */

typedef __INTPTR_TYPE__ intptr_t;
typedef __SIZE_TYPE__   size_t;

#if __cplusplus
#  define EXTERN_C extern "C"
#else
#  define EXTERN_C extern
#endif

EXTERN_C void* memcpy (void*, const void*, size_t);

void sink (const void*, ...);

char* nowarn_conditional (char *s)
{
  // Reduced from Glibc's tmpnam.c.
  extern char a[5];
  char b[5];
  char *p = s ? s : b;

  sink (p);

  if (s == 0)
    return a;

  return s;
}


char* nowarn_conditional_memcpy (char *s)
{
  // Reduced from Glibc's tmpnam.c.
  extern char a[5];
  char b[5];
  char *p = s ? s : b;

  sink (p);

  if (s == 0)
    return (char*)memcpy (a, p, 5);

  return s;
}


int warn_conditional_block (int i)
{
  int *p;
  if (i)
  {
    int a[] = { 1, 2, 3 };
    p = &a[i];
  }
  else
    p = &i;

  return *p;        // { dg-warning "dangling pointer \('p' \)to 'a' may be used" }
}
