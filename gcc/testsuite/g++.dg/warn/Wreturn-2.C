// { dg-do compile }
int foo(int first) {
  while (true) {
    return first;
  }
} // { dg-bogus "control reaches" }

