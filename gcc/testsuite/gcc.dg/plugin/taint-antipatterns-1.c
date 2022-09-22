/* { dg-do compile } */
// TODO: remove need for -fanalyzer-checker=taint here:
/* { dg-options "-fanalyzer -fanalyzer-checker=taint" } */
/* { dg-require-effective-target analyzer } */

#include "test-uaccess.h"

/* Adapted and simplified decls from linux kernel headers.  */

typedef unsigned char u8;
typedef unsigned __INT16_TYPE__ u16;
typedef unsigned __INT32_TYPE__ u32;
typedef signed __INT32_TYPE__ s32;
typedef __SIZE_TYPE__ size_t;

#define   EFAULT          14

typedef unsigned int gfp_t;
#define GFP_KERNEL 0

void kfree(const void *);
void *kmalloc(size_t size, gfp_t flags)
  __attribute__((malloc (kfree)));

/* Adapted from antipatterns.ko:taint.c (GPL-v2.0).   */

struct cmd_1
{
  u32 idx;
  u32 val;
};

static u32 arr[16];

int taint_array_access(void __user *src)
{
  struct cmd_1 cmd;
  if (copy_from_user(&cmd, src, sizeof(cmd)))
    return -EFAULT;
  /*
   * cmd.idx is an unsanitized value from user-space, hence
   * this is an arbitrary kernel memory access.
   */
  arr[cmd.idx] = cmd.val; /* { dg-warning "use of attacker-controlled value 'cmd.idx' in array lookup without upper-bounds checking" } */
  return 0;
}

struct cmd_2
{
  s32 idx;
  u32 val;
};

int taint_signed_array_access(void __user *src)
{
  struct cmd_2 cmd;
  if (copy_from_user(&cmd, src, sizeof(cmd)))
    return -EFAULT;
  if (cmd.idx >= 16)
    return -EFAULT;

  /*
   * cmd.idx hasn't been checked for being negative, hence
   * this is an arbitrary kernel memory access.
   */
  arr[cmd.idx] = cmd.val; /* { dg-warning "use of attacker-controlled value 'cmd.idx' in array lookup without checking for negative" } */
  return 0;
}

struct cmd_s32_binop
{
  s32 a;
  s32 b;
  s32 result;
};

int taint_divide_by_zero_direct(void __user *uptr)
{
  struct cmd_s32_binop cmd;
  if (copy_from_user(&cmd, uptr, sizeof(cmd)))
    return -EFAULT;

  /* cmd.b is attacker-controlled and could be zero */
  cmd.result = cmd.a / cmd.b; /* { dg-warning "use of attacker-controlled value 'cmd.b' as divisor without checking for zero" } */

  if (copy_to_user (uptr, &cmd, sizeof(cmd)))
    return -EFAULT;
  return 0;
}

int taint_divide_by_zero_compound(void __user *uptr)
{
  struct cmd_s32_binop cmd;
  if (copy_from_user(&cmd, uptr, sizeof(cmd)))
    return -EFAULT;

  /*
   * cmd.b is attacker-controlled and could be -1, hence
   * the divisor could be zero
   */
  cmd.result = cmd.a / (cmd.b + 1); /* { dg-warning "use of attacker-controlled value 'cmd.b \\+ 1' as divisor without checking for zero" } */

  if (copy_to_user (uptr, &cmd, sizeof(cmd)))
    return -EFAULT;
  return 0;
}

int taint_mod_by_zero_direct(void __user *uptr)
{
  struct cmd_s32_binop cmd;
  if (copy_from_user(&cmd, uptr, sizeof(cmd)))
    return -EFAULT;

  /* cmd.b is attacker-controlled and could be zero */
  cmd.result = cmd.a % cmd.b; /* { dg-warning "use of attacker-controlled value 'cmd.b' as divisor without checking for zero" } */

  if (copy_to_user (uptr, &cmd, sizeof(cmd)))
    return -EFAULT;
  return 0;
}

int taint_mod_by_zero_compound(void __user *uptr)
{
  struct cmd_s32_binop cmd;
  if (copy_from_user(&cmd, uptr, sizeof(cmd)))
    return -EFAULT;

  /*
   * cmd.b is attacker-controlled and could be -1, hence
   * the divisor could be zero
   */
  cmd.result = cmd.a % (cmd.b + 1); /* { dg-warning "use of attacker-controlled value 'cmd.b \\+ 1' as divisor without checking for zero" } */

  if (copy_to_user (uptr, &cmd, sizeof(cmd)))
    return -EFAULT;
  return 0;
}

/* TODO: etc.  */
