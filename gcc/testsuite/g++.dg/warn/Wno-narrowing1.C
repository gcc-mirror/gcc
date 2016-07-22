// PR c++/69872
// { dg-options "-Wall -Wextra -pedantic -Wno-narrowing" }

struct s { int x, y; };
short offsets[1] = {
  ((char*) &(((struct s*)16)->y) - (char *)16),  // { dg-bogus "note" }
};
