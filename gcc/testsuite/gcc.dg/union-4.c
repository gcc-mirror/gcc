/* { dg-do compile } */
/* { dg-options "-O -fdump-rtl-cse1" } */

extern void abort(void);

typedef unsigned int uint32;
typedef unsigned long long uint64;

typedef union {
  uint32 i32;
  uint64 i64;
} u64;

void foo(void)
{
  u64 data;
  data.i64 = 1;
  if (data.i32 != 1)
    abort ();
}

/* { dg-final { scan-rtl-dump-not "abort" "cse1" { target i?86-*-* x86_64-*-* } } } */
