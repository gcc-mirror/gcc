// PR c++/9993
// Bug: We were failing to destroy b.

// { dg-do run }

int c, d;

struct Object {
  Object()                      { ++c; }
  Object(const Object&)         { ++c; }
  ~Object()                     { ++d; }
};

Object function() {
  int i = 0;
  do {
    Object b;
    if (i++ == 2)
      return b;
  } while (1);
}

int main() {
  function();
  return c != d;
}
