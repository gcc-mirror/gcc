// PR c++/55137
// { dg-do compile }

enum E { F = -1 + (int) (sizeof (int) - 1) };
