// PR c++/14211

void f(char *str) {
  char *& m = const_cast<char *>(str); // { dg-error "" }
}
