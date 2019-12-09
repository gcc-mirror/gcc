/* { dg-do compile } */
/* { dg-options "-O2 -Wall -Wno-array-bounds -ftrack-macro-expansion=0" } */

extern void abort (void);

#include "../gcc.c-torture/execute/builtins/chk.h"

extern char *mallocminus1(int size) __attribute__((alloc_size(-1))); /* { dg-warning ".alloc_size. attribute argument value .-1. exceeds the number of function parameters 1" } */
extern char *malloc0(int size) __attribute__((alloc_size(0))); /* { dg-warning ".alloc_size. attribute argument value .0. does not refer to a function parameter" } */
extern char *malloc1(int size) __attribute__((alloc_size(1)));
extern char *malloc2(int empty, int size) __attribute__((alloc_size(2)));
extern char *calloc1(int size, int elements) __attribute__((alloc_size(1,2)));
extern char *calloc2(int size, int empty, int elements) __attribute__((alloc_size(1,3)));
extern char *balloc1(void *size) __attribute__((alloc_size(1)));   /* { dg-warning ".alloc_size. attribute argument value .1. refers to parameter type .void *." } */

void
test (void)
{
  char *p;

  p = malloc0 (6);
  strcpy (p, "Hello");
  p = malloc1 (6);
  strcpy (p, "Hello");
  strcpy (p, "Hello World"); /* { dg-warning "writing" "strcpy" } */
  p = malloc2 (__INT_MAX__ >= 1700000 ? 424242 : __INT_MAX__ / 4, 6);
  strcpy (p, "World");
  strcpy (p, "Hello World"); /* { dg-warning "writing" "strcpy" } */
  p = calloc1 (2, 5);
  strcpy (p, "World");
  strcpy (p, "Hello World"); /* { dg-warning "writing" "strcpy" } */
  p = calloc2 (2, __INT_MAX__ >= 1700000 ? 424242 : __INT_MAX__ / 4, 5);
  strcpy (p, "World");
  strcpy (p, "Hello World"); /* { dg-warning "writing" "strcpy" } */
}

