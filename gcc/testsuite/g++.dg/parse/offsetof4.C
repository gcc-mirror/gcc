// { dg-do compile }
// Contributed by Bill Helfinstine <bhelf at flitterfly dot whirpon dot com>
// PR c++/14932: Allow subscript operator in offsetof

#include <cstddef>

struct A
{
  int bar;
  int foo[22];
};

const int off = offsetof(A, foo[12]);
