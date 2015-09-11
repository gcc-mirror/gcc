/* { dg-do compile } */
/* { dg-options "-O2 -fcheck-pointer-bounds -mmpx" } */

#pragma interface

struct S
{
  ~S ()
  {
  }
};

S s;
