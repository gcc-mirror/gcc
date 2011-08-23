// { dg-options "-std=c++0x" }

int main() {
  int i = 0;
  int& r = [&] () { return i; } (); // { dg-error "" "invalid initialization of non-const reference of type .int&. from a temporary of type .int." }

  return 0;
}

