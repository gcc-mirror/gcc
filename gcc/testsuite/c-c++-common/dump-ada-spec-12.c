/* { dg-do compile } */
/* { dg-options "-fdump-ada-spec" } */

struct S1
{
  enum { Blue, Red, Green } E;
};

struct S2
{
  enum { One = 1, Two, Three } E;
};

/* { dg-final { cleanup-ada-spec } } */
