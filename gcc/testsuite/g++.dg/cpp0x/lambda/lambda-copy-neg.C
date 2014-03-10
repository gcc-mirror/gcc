// { dg-do compile { target c++11 } }

int main() {
  int i;
  const char* s;
  [i, s] () -> void { i; s; } ();

  [] () -> void { i; } (); // { dg-error "" "`i' is not captured" }
  [1] () -> void {} (); // { dg-error "expected identifier" }

  return 0;
}

