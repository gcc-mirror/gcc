/* Adapted and simplified decls from linux kernel headers.  */

/* { dg-do compile } */
/* { dg-options "-fanalyzer" } */
/* { dg-require-effective-target analyzer } */
/* { dg-skip-if "structure layout assumption not met" { default_packed } } */

typedef unsigned char u8;
typedef unsigned __INT16_TYPE__ u16;
typedef unsigned __INT32_TYPE__ u32;
typedef __SIZE_TYPE__ size_t;

#define   EFAULT          14

#include "test-uaccess.h"

typedef unsigned int gfp_t;
#define GFP_KERNEL 0

void kfree(const void *);
void *kmalloc(size_t size, gfp_t flags)
  __attribute__((malloc (kfree)));

/* Adapted from antipatterns.ko:infoleak.c (GPL-v2.0).   */

struct infoleak_buf
{
  char buf[256];
};

int infoleak_stack_no_init(void __user *dst)
{
  struct infoleak_buf st; /* { dg-message "region created on stack here" "where" } */
  /* { dg-message "capacity: 256 bytes" "capacity" { target *-*-* } .-1 } */
  
  /* No initialization of "st" at all.  */
  if (copy_to_user(dst, &st, sizeof(st))) /* { dg-warning "potential exposure of sensitive information by copying uninitialized data from stack" "warning" } */
    /* { dg-message "256 bytes are uninitialized" "note how much" { target *-*-* } .-1 } */
    return -EFAULT;
  return 0;
}

int infoleak_heap_no_init(void __user *dst)
{
  struct infoleak_buf *heapbuf = kmalloc(sizeof(*heapbuf), GFP_KERNEL);
  /* No initialization of "heapbuf" at all.  */

  /* TODO: we also don't check that heapbuf could be NULL when copying
     from it.  */
  if (copy_to_user(dst, heapbuf, sizeof(*heapbuf))) /* { dg-warning "exposure" "warning" { xfail *-*-* } } */
    /* TODO(xfail).  */
    return -EFAULT; /* { dg-warning "leak of 'heapbuf'" } */

  kfree(heapbuf);
  return 0;
}

struct infoleak_2
{
  u32 a;
  u32 b; /* { dg-message "field 'b' is uninitialized \\(4 bytes\\)" } */
};

int infoleak_stack_missing_a_field(void __user *dst, u32 v)
{
  struct infoleak_2 st; /* { dg-message "region created on stack here" "where" } */
  /* { dg-message "capacity: 8 bytes" "capacity" { target *-*-* } .-1 } */
  
  st.a = v;
  /* No initialization of "st.b".  */
  if (copy_to_user(dst, &st, sizeof(st))) /* { dg-warning "potential exposure of sensitive information by copying uninitialized data from stack" "warning" } */
    /* { dg-message "4 bytes are uninitialized" "note how much" { target *-*-* } .-1 } */
    return -EFAULT;
  return 0;
}

int infoleak_heap_missing_a_field(void __user *dst, u32 v)
{
  struct infoleak_2 *heapbuf = kmalloc(sizeof(*heapbuf), GFP_KERNEL);
  heapbuf->a = v; /* { dg-warning "dereference of possibly-NULL 'heapbuf'" } */
  /* No initialization of "heapbuf->b".  */
  if (copy_to_user(dst, heapbuf, sizeof(*heapbuf))) /* { dg-warning "exposure" "warning" { xfail *-*-* } } */
    /* TODO(xfail).  */
    {
      kfree(heapbuf);
      return -EFAULT;
    }
  kfree(heapbuf);
  return 0;
}

struct infoleak_3
{
  u8 a; /* { dg-message "padding after field 'a' is uninitialized \\(3 bytes\\)" } */
  /* padding here */
  u32 b;
};

int infoleak_stack_padding(void __user *dst, u8 p, u32 q)
{
  struct infoleak_3 st; /* { dg-message "region created on stack here" "where" } */
  /* { dg-message "capacity: 8 bytes" "capacity" { target *-*-* } .-1 } */

  st.a = p;
  st.b = q;
  /* No initialization of padding.  */
  if (copy_to_user(dst, &st, sizeof(st))) /* { dg-warning "potential exposure of sensitive information by copying uninitialized data from stack" "warning" } */
    /* { dg-message "3 bytes are uninitialized" "note how much" { target *-*-* } .-1 } */
    return -EFAULT;
  return 0;
}

int infoleak_stack_unchecked_err(void __user *dst, void __user *src)
{
  struct infoleak_buf st;  /* { dg-message "region created on stack here" "where" } */
  /* { dg-message "capacity: 256 bytes" "capacity" { target *-*-* } .-1 } */

  /*
   * If the copy_from_user call fails, then st is still uninitialized,
   * and if the copy_to_user call succeds, we have an infoleak.
   */
  int err = copy_from_user (&st, src, sizeof(st)); /* { dg-message "when 'copy_from_user' fails" } */
  err |= copy_to_user (dst, &st, sizeof(st)); /* { dg-warning "exposure" "warning" } */
  /* { dg-message "256 bytes are uninitialized" "note how much" { target *-*-* } .-1 } */
  /* Actually, it's *up to* 256 bytes.  */

  if (err)
    return -EFAULT;
  return 0;
}

struct infoleak_4
{
  union {
    u8 f1;
    u32 f2;
  } u;
};

int infoleak_stack_union(void __user *dst, u8 v)
{
  struct infoleak_4 st;
  /*
   * This write only initializes the u8 within the union "u",
   * leaving the remaining 3 bytes uninitialized.
   */
  st.u.f1 = v;
  if (copy_to_user(dst, &st, sizeof(st))) /* { dg-warning "potential exposure of sensitive information by copying uninitialized data from stack" "warning" } */
    /* { dg-message "3 bytes are uninitialized" "note how much" { target *-*-* } .-1 } */
    return -EFAULT;
  return 0;
}

struct infoleak_5
{
  void *ptr;
};

int infoleak_stack_kernel_ptr(void __user *dst, void *kp)
{
  struct infoleak_5 st;
  /* This writes a kernel-space pointer into a user space buffer.  */
  st.ptr = kp;
  if (copy_to_user(dst, &st, sizeof(st))) // TODO: we don't complain about this yet
    return -EFAULT;
  return 0;
}
