/* PR c/53037.  */
/* { dg-do compile } */
/* { dg-options "-O0" } */
/* { dg-require-effective-target int32 } */

typedef unsigned long long __u64
  __attribute__((aligned(4),warn_if_not_aligned(8)));

struct foo1
{
  int i1;
  int i2;
  int i3;
  __u64 x; /* { dg-warning "'x' offset 12 in 'struct foo1' isn't aligned to 8" } */
}; /* { dg-warning "alignment 4 of 'struct foo1' is less than 8" } */

struct foo2
{
  int i1;
  int i2;
  int i3;
  __u64 x; /* { dg-warning "'x' offset 12 in 'struct foo2' isn't aligned to 8" } */
} __attribute__((aligned(8)));

struct foo3
{
  int i1;
  int i3;
  __u64 x;
}; /* { dg-warning "alignment 4 of 'struct foo3' is less than 8" } */

struct foo4
{
  int i1;
  int i2;
  __u64 x;
} __attribute__((aligned(8)));

struct foo5
{
  int i1;
  int x __attribute__((warn_if_not_aligned(16))); /* { dg-warning "'x' offset 4 in 'struct foo5' isn't aligned to 16" } */
}; /* { dg-warning {alignment [0-9]+ of 'struct foo5' is less than 16} } */

struct foo6
{
  int i1;
  int x __attribute__((warn_if_not_aligned(16))); /* { dg-warning "'x' offset 4 in 'struct foo6' isn't aligned to 16" } */
} __attribute__((aligned(16)));

struct foo7
{
  int i1;
  int i2;
  int i3;
  int i4;
  int x __attribute__((warn_if_not_aligned(16)));
} __attribute__((aligned(16)));

union bar1
{
  int i1;
  __u64 x;
}; /* { dg-warning "alignment 4 of 'union bar1' is less than 8" } */

union bar2
{
  int i1;
  __u64 x;
} __attribute__((aligned(8)));

union bar3
{
  int i1;
  int x __attribute__((warn_if_not_aligned(16))); 
}; /* { dg-warning {alignment [0-9]+ of 'union bar3' is less than 16} } */

union bar4
{
  int i1;
  int x __attribute__((warn_if_not_aligned(16)));
} __attribute__((aligned(16)));
