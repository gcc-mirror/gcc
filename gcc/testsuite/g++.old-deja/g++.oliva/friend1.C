// { dg-do assemble  }

// Copyright (C) 2001 Free Software Foundation

// by Alexandre Oliva <aoliva@redhat.com>

// We shouldn't warn about bar referring to a non-template in this case.

template <typename T>
class foo {
  friend int bar(int);
};
