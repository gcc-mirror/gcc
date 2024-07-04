// PR c++/113853
// { dg-do compile { target c++20 } }

struct X {
  X();
  X(const X&);
  X(X&&) = delete;
};

void
f1 ()
{
  try {
    ;
  } catch (X x) {
    throw x; // { dg-error "use of deleted function" }
  }
}

void
f2 (X x)
try {
  ;
} catch (...) {
  throw x;  // { dg-error "use of deleted function" }
}

void
f2b (X x)
try {
  ;
} catch (...) {
  {
    throw x;  // { dg-error "use of deleted function" }
  }
}

void
f3 ()
try {
  X x;
  throw x;  // { dg-error "use of deleted function" }
} catch (...) {
}

void
f3b ()
try {
  {
    X x;
    throw x;  // { dg-error "use of deleted function" }
  }
} catch (...) {
}

void
f4 (X x)
try {
  throw x;
} catch (...) {
}

void
f4b (X x)
try {
  {
    throw x;
  }
} catch (...) {
}

void
f5 (X x)
{
  void g (decltype(throw x, true));	// { dg-error "use of deleted function|expected" }
}

// The "expected" shouldn't be here, c++/113924.
void
f6 (X x, int = decltype(throw x, true){}) // { dg-error "use of deleted function|expected" }
{
}

void
f7 (X x)
{
  [&] { throw x; };
}
