// PR c++/55951
// { dg-do compile }
// { dg-options "" }

enum { A };

static const char *a[] = {
  [A] = "a"
};
