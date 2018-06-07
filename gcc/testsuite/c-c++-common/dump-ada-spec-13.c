/* { dg-do compile } */
/* { dg-options "-fdump-ada-spec" } */

struct S1
{
  enum T1 { Blue, Red, Green } E;
};

struct S2
{
  enum T2 { One = 1, Two, Three } E;
};

/* { dg-final { cleanup-ada-spec } } */
