/* { dg-do compile } */
/* { dg-options "-fanalyzer" } */
/* { dg-require-effective-target analyzer } */

#include <string.h>

#include "test-uaccess.h"

typedef unsigned char u8;
typedef unsigned __INT16_TYPE__ u16;
typedef unsigned __INT32_TYPE__ u32;

struct s1
{
  u32 i;
};

void test_1a (void __user *dst, u32 a)
{
  struct s1 s;
  s.i = a;
  copy_to_user(dst, &s, sizeof (struct s1)); /* { dg-bogus "" } */
}

void test_1b (void __user *dst, u32 a)
{
  struct s1 s;
  copy_to_user(dst, &s, sizeof (struct s1)); /* { dg-warning "potential exposure of sensitive information by copying uninitialized data from stack" "warning" } */
  /* { dg-message "4 bytes are uninitialized" "note how much" { target *-*-* } .-1 } */
}

void test_1c (void __user *dst, u32 a)
{
  struct s1 s;
  memset (&s, 0, sizeof (struct s1));
  copy_to_user(dst, &s, sizeof (struct s1)); /* { dg-bogus "" } */
}

void test_1d (void __user *dst, u32 a)
{
  struct s1 s = {0};
  copy_to_user(dst, &s, sizeof (struct s1)); /* { dg-bogus "" } */
}

struct s2
{
  u32 i;
  u32 j; /* { dg-message "field 'j' is uninitialized \\(4 bytes\\)" } */
};

void test_2a (void __user *dst, u32 a)
{
  struct s2 s; /* { dg-message "region created on stack here" "where" } */
  /* { dg-message "capacity: 8 bytes" "capacity" { target *-*-* } .-1 } */
  s.i = a;
  copy_to_user(dst, &s, sizeof (struct s2)); /* { dg-warning "potential exposure of sensitive information by copying uninitialized data from stack" "warning" } */
  /* { dg-message "4 bytes are uninitialized" "note how much" { target *-*-* } .-1 } */
}

void test_2b (void __user *dst, u32 a)
{
  struct s2 s;
  s.i = a;
  /* Copy with wrong size (only part of s2).  */
  copy_to_user(dst, &s, sizeof (struct s1));
}

void test_2d (void __user *dst, u32 a)
{
  struct s2 s = {0};
  s.i = a;
  copy_to_user(dst, &s, sizeof (struct s2)); /* { dg-bogus" } */
}

struct empty {};

void test_empty (void __user *dst)
{
  struct empty e;
  copy_to_user(dst, &e, sizeof (struct empty));
}

union un_a
{
  u32 i;
  u8  j;
};

/* As above, but in a different order.  */

union un_b
{
  u8  j;
  u32 i;
};

void test_union_1a (void __user *dst, u8 v)
{
  union un_a u; /* { dg-message "region created on stack here" "where" } */
  /* { dg-message "capacity: 4 bytes" "capacity" { target *-*-* } .-1 } */
  u.j = v;
  copy_to_user(dst, &u, sizeof (union un_a)); /* { dg-warning "potential exposure of sensitive information by copying uninitialized data from stack" "warning" } */
  /* { dg-message "3 bytes are uninitialized" "note how much" { target *-*-* } .-1 } */
  /* { dg-message "bytes 1 - 3 are uninitialized" "note how much" { target *-*-* } .-2 } */
}

void test_union_1b (void __user *dst, u8 v)
{
  union un_b u; /* { dg-message "region created on stack here" "where" } */
  /* { dg-message "capacity: 4 bytes" "capacity" { target *-*-* } .-1 } */
  u.j = v;
  copy_to_user(dst, &u, sizeof (union un_b)); /* { dg-warning "potential exposure of sensitive information by copying uninitialized data from stack" "warning" } */
  /* { dg-message "3 bytes are uninitialized" "note how much" { target *-*-* } .-1 } */
  /* { dg-message "bytes 1 - 3 are uninitialized" "note how much" { target *-*-* } .-2 } */
}

void test_union_2a (void __user *dst, u8 v)
{
  union un_a u = {0};
  u.j = v;
  copy_to_user(dst, &u, sizeof (union un_a));
}

void test_union_2b (void __user *dst, u8 v)
{
  union un_b u = {0};
  u.j = v;
  copy_to_user(dst, &u, sizeof (union un_b));
}

void test_union_3a (void __user *dst, u32 v)
{
  union un_a u;
  u.i = v;
  copy_to_user(dst, &u, sizeof (union un_a)); /* { dg-bogus "" } */
}

void test_union_3b (void __user *dst, u32 v)
{
  union un_b u;
  u.i = v;
  copy_to_user(dst, &u, sizeof (union un_b)); /* { dg-bogus "" } */
}

void test_union_4a (void __user *dst, u8 v)
{
  union un_a u = {0};
  copy_to_user(dst, &u, sizeof (union un_a)); /* { dg-bogus "" } */
}

void test_union_4b (void __user *dst, u8 v)
{
  union un_b u = {0};
  copy_to_user(dst, &u, sizeof (union un_b)); /* { dg-bogus "" } */
}

struct st_union_5
{
  union {
    u8 f1;
    u32 f2;
  } u; /* { dg-message "field 'u' is partially uninitialized" } */
};

void test_union_5 (void __user *dst, u8 v)
{
  struct st_union_5 st; /* { dg-message "region created on stack here" "where" } */
  /* { dg-message "capacity: 4 bytes" "capacity" { target *-*-* } .-1 } */

  /* This write only initializes the u8 within the union "u",
     leaving the remaining 3 bytes uninitialized.  */
  st.u.f1 = v;

  copy_to_user (dst, &st, sizeof(st)); /* { dg-warning "potential exposure of sensitive information by copying uninitialized data from stack" "warning" } */
  /* { dg-message "3 bytes are uninitialized" "note how much" { target *-*-* } .-1 } */
}

void test_one_byte (void __user *dst)
{
  char src;  /* { dg-message "region created on stack here" "where" } */
  /* { dg-message "capacity: 1 byte" "capacity" { target *-*-* } .-1 } */

  copy_to_user (dst, &src, sizeof(src)); /* { dg-warning "potential exposure of sensitive information by copying uninitialized data from stack" "warning" } */
  /* { dg-message "1 byte is uninitialized" "note how much" { target *-*-* } .-1 } */
}
