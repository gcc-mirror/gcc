// Copyright (C) 2007 Free Software Foundation
// Contributed by Ollie Wild <aaw@google.com>
// { dg-do run }

// Test for proper conversion of null pointers to data members.

struct B1 {
  int x;
};

struct B2 {
  int x;
};

struct D : B1, B2 {
  int x;
};

int main ()
{
  int D::*pd = 0;
  int B2::*pb2 = 0;

  return pd != pb2;
}
