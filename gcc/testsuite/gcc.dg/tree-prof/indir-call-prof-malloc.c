/* { dg-options "-O2 -ldl -fprofile-correction" } */

#define _GNU_SOURCE
#include <stdio.h>
#include <stdint.h>
#include <dlfcn.h>

int global;
int global2;

void report1 (size_t size)
{
  global++;
}

void report2 (size_t size)
{
  global2++;
}

typedef void (*tp) (size_t);
static tp fns[] = {report1, report2};

void* malloc(size_t size)
{
  static void* (*real_malloc)(size_t) = NULL;
  if (!real_malloc)
      real_malloc = dlsym(RTLD_NEXT, "malloc");

  void *p = real_malloc (size);
  fns[size % 2] (size);
  // fprintf(stderr, "malloc(%d) = %p\n", size, p);
  return p;
}

void *calloc (size_t n, size_t size)
{
  void *ptr = malloc (n * size);
  __builtin_memset (ptr, 0, n * size);
  return ptr;
}

void *ptr;

int main()
{
  ptr = malloc (16);
  return 0;
}
