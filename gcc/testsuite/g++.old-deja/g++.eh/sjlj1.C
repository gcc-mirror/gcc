// Special g++ Options: -fsjlj-exceptions

// execution test - XFAIL *-*-*

void foo() throw(int) { throw 1; }

int main() {
  try { foo(); }
  catch(int) { return 0; }
  abort();
}
