// { dg-options "-std=c++0x" }

int main() {
  int i;
  const char* s;
  [=] () -> void { i; s; i; s; } ();

  [] () -> void { i; } (); // { dg-error "" "`i' is not captured" }
  [1] () -> void {} (); // { dg-error "expected identifier" }

  return 0;
}

