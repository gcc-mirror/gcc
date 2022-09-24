/* Verify that -Wanalyzer-exposure-through-uninit-copy doesn't get confused
   if size argument to copy_to_user is an upper bound, rather than a
   constant.  */

/* { dg-do compile } */
/* { dg-options "-fanalyzer" } */
/* { dg-require-effective-target analyzer } */

#include "../analyzer/analyzer-decls.h"

typedef __SIZE_TYPE__ size_t;

#include "test-uaccess.h"

typedef unsigned __INT32_TYPE__ u32;

/* min_t adapted from include/linux/kernel.h.  */

#define min_t(type, x, y) ({			\
	type __min1 = (x);			\
	type __min2 = (y);			\
	__min1 < __min2 ? __min1: __min2; })

struct st
{
  u32 a;
  u32 b;
};

/* Verify that we cope with min_t.  */

void test_1_full_init (void __user *dst, u32 x, u32 y, unsigned long in_sz)
{
  struct st s;
  s.a = x;
  s.b = y;
  unsigned long copy_sz = min_t(unsigned long, in_sz, sizeof(s));
  copy_to_user(dst, &s, copy_sz); /* { dg-bogus "exposure" } */
}

void test_1_partial_init (void __user *dst, u32 x, u32 y, unsigned long in_sz)
{
  struct st s;
  s.a = x;
  /* s.y not initialized.  */
  unsigned long copy_sz = min_t(unsigned long, in_sz, sizeof(s));
  copy_to_user(dst, &s, copy_sz); /* { dg-warning "exposure" } */
}

/* Constant on LHS rather than RHS.  */

void test_2_full_init (void __user *dst, u32 x, u32 y, unsigned long in_sz)
{
  struct st s;
  s.a = x;
  s.b = y;
  unsigned long copy_sz = min_t(unsigned long, sizeof(s), in_sz);
  copy_to_user(dst, &s, copy_sz); /* { dg-bogus "exposure" } */
}

void test_2_partial_init (void __user *dst, u32 x, u32 y, unsigned long in_sz)
{
  struct st s;
  s.a = x;
  /* s.y not initialized.  */
  unsigned long copy_sz = min_t(unsigned long, sizeof(s), in_sz);
  copy_to_user(dst, &s, copy_sz); /* { dg-warning "exposure" } */
}

/* min_t with various casts.  */

void test_3_full_init (void __user *dst, u32 x, u32 y, int in_sz)
{
  struct st s;
  s.a = x;
  s.b = y;
  int copy_sz = min_t(unsigned int, in_sz, sizeof(s));
  copy_to_user(dst, &s, copy_sz); /* { dg-bogus "exposure" } */
}

void test_3_partial_init (void __user *dst, u32 x, u32 y, int in_sz)
{
  struct st s;
  s.a = x;
  /* s.y not initialized.  */
  int copy_sz = min_t(unsigned int, in_sz, sizeof(s));
  copy_to_user(dst, &s, copy_sz); /* { dg-warning "exposure" } */
}

/* Comparison against an upper bound.  */

void test_4_full_init (void __user *dst, u32 x, u32 y, size_t in_sz)
{
  struct st s;
  s.a = x;
  s.b = y;
  
  size_t copy_sz = in_sz;
  if (copy_sz > sizeof(s))
    copy_sz = sizeof(s);

  copy_to_user(dst, &s, copy_sz); /* { dg-bogus "exposure" } */
}

void test_4_partial_init (void __user *dst, u32 x, u32 y, size_t in_sz)
{
  struct st s;
  s.a = x;
  /* s.y not initialized.  */
  
  size_t copy_sz = in_sz;
  if (copy_sz > sizeof(s))
    copy_sz = sizeof(s);

  copy_to_user(dst, &s, copy_sz); /* { dg-warning "exposure" } */
}

/* Comparison against an upper bound with casts.  */

void test_5_full_init (void __user *dst, u32 x, u32 y, int in_sz)
{
  struct st s;
  s.a = x;
  s.b = y;
  
  int copy_sz = in_sz;
  if (copy_sz > sizeof(s))
    copy_sz = sizeof(s);
  copy_to_user(dst, &s, copy_sz); /* { dg-bogus "exposure" } */
}

/* Comparison against an upper bound with casts.  */

void test_5_partial_init (void __user *dst, u32 x, u32 y, int in_sz)
{
  struct st s;
  s.a = x;
  /* s.y not initialized.  */
  
  int copy_sz = in_sz;
  if (copy_sz > sizeof(s))
    copy_sz = sizeof(s);

  copy_to_user(dst, &s, copy_sz); /* { dg-warning "exposure" "" } */
}
