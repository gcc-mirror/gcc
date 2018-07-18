// PR c/7652
// { dg-do compile }
// { dg-options "-std=c++17 -Wextra -Wall -Wpedantic" }

// Check that we accept attribute [[fallthrough]].

extern void bar (int);

void
f (int i)
{
  switch (i)
    {
    case 1:
      bar (1);
      [[fallthrough]];
    case 2:
      bar (2);
    }
}
