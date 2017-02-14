// PR c/7652
// { dg-do compile { target c++11 } }
// { dg-options "-Wextra -Wall -Wpedantic" }

extern void bar (int);

void
f (int i)
{
  switch (i)
    {
    case 1:
      bar (1);
      [[fallthrough]]; // { dg-warning ".fallthrough. is a C\\+\\+17 feature" "" { target { c++14_down } }  }
    case 3:
      bar (1);
      [[gnu::fallthrough, gnu::fallthrough]]; // { dg-warning ".fallthrough. attribute specified multiple times" }
    case 2:
      bar (2);
    }
}
