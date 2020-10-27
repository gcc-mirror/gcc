/* PR gcov-profile/97461 */
/* { dg-options "-O2 -ldl" } */

#define _GNU_SOURCE

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static int malloc_depth = 0;

static char memory[128* 1024];
static size_t memory_p = 0;

void f1(void) {}
void f2(void) {}

typedef void (*fun_t)(void);
static const fun_t funs[2] = { f1, f2, };

static void * malloc_impl(size_t size) {
    void * r = &memory[memory_p];
    memory_p += size;

    // force TOPN profile
    funs[size % 2]();
    return r;
}

// Override default malloc, check it it get s called recursively
void * malloc(size_t size) {
    // Must not be called recursively. Malloc implementation does not support it.
    if (malloc_depth != 0) __builtin_trap();

    ++malloc_depth;
      void * r = malloc_impl(size);
    --malloc_depth;
    return r;
}

// Called from gcov
void *calloc(size_t nmemb, size_t size) {
    // Must not be called recursively.  Malloc implementation does not support it.
    if (malloc_depth != 0) __builtin_trap();

    ++malloc_depth;
      void * r = malloc_impl(size * nmemb);
      memset(r, 0, size * nmemb);
    --malloc_depth;
    return r;
}

void free(void *ptr){}

int main() {
    void * p = malloc(8);
    return p != 0 ? 0 : 1;
}
