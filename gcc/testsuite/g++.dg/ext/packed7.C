// PR c++/14124
// A packed enum uses the minimal underlying type.

// Copyright (C) 2004 Free Software Foundation, Inc.
// Contributed by Matt Austern  <austern@apple.com>

// { dg-do run }

enum XXX { xyzzy = 3 } __attribute__((packed));

int main()
{
  int enumsize = sizeof(xyzzy);
  return (enumsize == 1) ? 0 : 1;
}
