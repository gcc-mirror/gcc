// PR target/29487
// { dg-require-weak "" }
// { dg-do run { xfail "hppa*-hp-hpux11.*" } }
// { dg-additional-sources "weak1-a.cc" }
// { dg-options "-O2" }

extern __attribute__((weak)) 
void f() {
}

int main () {
  try {
    f();
    return 1;
  } catch (int i) {
    /* Although the implementation of f in this file does not throw
       any exceptions, it is weak, and may therefore be replaced at
       link time.  Therefore, the compiler must not optimize away this
       catch clause.  */
    if (i != 7)
      return 2;
  }
}
