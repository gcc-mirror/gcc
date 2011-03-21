// { dg-do run }
// { dg-options "-std=c++0x" }

int main() {
  int i;
  const char* s;
  [i, s] () -> void { i; s; } ();

  //[] () -> void { i; } (); // { dg-error: "`i' is not in scope" }
  //[1] () -> void {} (); // { dg-error: "expected identifier" }

  return 0;
}

