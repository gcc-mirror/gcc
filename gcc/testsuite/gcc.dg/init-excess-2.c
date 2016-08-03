/* Test for diagnostics about excess initializers when using a macro
   defined in a system header:
   c/71115 - Missing warning: excess elements in struct initializer.  */
/* { dg-do compile } */
/* { dg-options "" } */
/* { dg-require-effective-target int32plus } */

#include <stddef.h>

int* a[1] = {
  0,
  NULL              /* { dg-warning "excess elements|near init" } */
};

const char str[1] = {
  0,
  NULL              /* { dg-warning "excess elements|near init" } */
};

struct S {
  int *a;
} s = {
  0,
  NULL              /* { dg-warning "excess elements|near init" } */
};

struct __attribute__ ((designated_init)) S2 {
  int *a;
} s2 = {
  NULL              /* { dg-warning "positional initialization|near init" } */
};

union U {
  int *a;
} u = {
  0,
  NULL              /* { dg-warning "excess elements|near init" } */
};

int __attribute__ ((vector_size (16))) ivec = {
  0, 0, 0, 0,
  NULL              /* { dg-warning "excess elements|near init" } */
};

int* scal = {
  0,
  NULL              /* { dg-warning "excess elements|near init" } */
};
