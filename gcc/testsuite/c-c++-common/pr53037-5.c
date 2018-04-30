/* PR c/53037.  */
/* { dg-do compile } */
/* { dg-options "-O0 -Wno-if-not-aligned" } */

typedef unsigned long long __u64
  __attribute__((aligned(4),warn_if_not_aligned(8)));

struct foo1
{
  int i1;
  int i2;
  int i3;
  __u64 x;
};

struct foo2
{
  int i1;
  int i2;
  int i3;
  __u64 x;
} __attribute__((aligned(8)));

struct foo3
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

struct foo5
{
  int i1;
  int x __attribute__((warn_if_not_aligned(16)));
};

struct foo6
{
  int i1;
  int x __attribute__((warn_if_not_aligned(16))); 
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
};

union bar2
{
  int i1;
  __u64 x;
} __attribute__((aligned(8)));

union bar3
{
  int i1;
  int x __attribute__((warn_if_not_aligned(16))); 
};

union bar4
{
  int i1;
  int x __attribute__((warn_if_not_aligned(16)));
} __attribute__((aligned(16)));
