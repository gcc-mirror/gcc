// PR c++/117099
// With -fpermissive, make sure we don't do NRV in this case, but keep
// executing fine. Note that the return at line 16 is undefined behaviour.
// { dg-do "run" }
// { dg-options "-fpermissive" }

struct X {
  ~X() {}
};

X test(bool b) {
  X x;
  if (b)
    return x;
  else
    return; // { dg-warning "return-statement with no value" }
}

int main(int, char*[]) {
  (void) test (false);
  return 0;
}
