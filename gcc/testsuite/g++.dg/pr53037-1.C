/* PR c/53037.  */
/* { dg-do compile } */
/* { dg-options "-O0" } */

typedef unsigned long long __u64
  __attribute__((aligned(4),warn_if_not_aligned(8)));

struct foo1 /* { dg-warning "alignment 4 of 'foo1' is less than 8" } */
{
  int i1;
  int i2;
  int i3;
  __u64 x; /* { dg-warning "'foo1::x' offset 12 in 'foo1' isn't aligned to 8" } */
};

struct foo2
{
  int i1;
  int i2;
  int i3;
  __u64 x; /* { dg-warning "'foo2::x' offset 12 in 'foo2' isn't aligned to 8" } */
} __attribute__((aligned(8)));

struct foo3 /* { dg-warning "alignment 4 of 'foo3' is less than 8" } */
{
  int i1;
  int i3;
  __u64 x;
};

struct foo4
{
  int i1;
  int i2;
  __u64 x;
} __attribute__((aligned(8)));

struct foo5 /* { dg-warning "alignment \[0-9\]+ of 'foo5' is less than 16" } */
{
  int i1;
  int x __attribute__((warn_if_not_aligned(16))); /* { dg-warning "'foo5::x' offset 4 in 'foo5' isn't aligned to 16" } */
};

struct foo6
{
  int i1;
  int x __attribute__((warn_if_not_aligned(16))); /* { dg-warning "'foo6::x' offset 4 in 'foo6' isn't aligned to 16" } */
} __attribute__((aligned(16)));

struct foo7
{
  int i1;
  int i2;
  int i3;
  int i4;
  int x __attribute__((warn_if_not_aligned(16)));
} __attribute__((aligned(16)));

union bar1 /* { dg-warning "alignment 4 of 'bar1' is less than 8" } */
{
  int i1;
  __u64 x;
};

union bar2
{
  int i1;
  __u64 x;
} __attribute__((aligned(8)));

union bar3 /* { dg-warning "alignment \[0-9\]+ of 'bar3' is less than 16" } */
{
  int i1;
  int x __attribute__((warn_if_not_aligned(16))); 
};

union bar4
{
  int i1;
  int x __attribute__((warn_if_not_aligned(16)));
} __attribute__((aligned(16)));
