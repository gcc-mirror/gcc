// PR target/29487
// { dg-do link }
// { dg-options "-O2" }

/* This function is not defined.  The compiler should optimize away
   all calls to it.  */
extern void undefined () throw ();

extern void f1();

inline void f2() {
  f1();
}

/* This function will be COMDAT if not inlined.  */
inline void f1() {}

/* This function will be COMDAT.  */
template <typename T>
void f3() {
  if (false)
    throw 3;
}

inline void f4() {
  if (false)
    throw 7;
}

int main () {
  try {
    f1();
    f2();
    f3<int>();
    f4();
  } catch (...) {
    /* The compiler should recognize that none of the functions above
       can throw exceptions, and therefore remove this code as
       unreachable.  */
    undefined ();
  }
}
