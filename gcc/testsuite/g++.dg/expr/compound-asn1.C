// PR c++/45437
// { dg-options -Wsequence-point }
// { dg-do run }

bool f(bool& b) {
  b = true;
  return false;
}

int main() {
  bool b = false;
  b |= f(b);
  if (!b)
    return 1;
}
