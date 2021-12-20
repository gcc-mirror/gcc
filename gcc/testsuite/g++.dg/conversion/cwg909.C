// Per CWG 909, the two casts below have the same validity and meaning, since:
// "one possible interpretation of an old-style cast is as a static_cast followed by a const_cast."

struct S {
  operator const int* ();
};

void f(S& s)  {
  const_cast<int*>(static_cast<const int*>(s));  // #1
  (int*) s;  // #2
}
