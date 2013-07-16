// PR c++/55951

enum { A };

static const char *a[] = {
  [A] = "a"
};
