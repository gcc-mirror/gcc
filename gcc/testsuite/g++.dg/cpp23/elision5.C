// PR c++/101165 - P2266R1 - Simpler implicit move
// { dg-do compile { target c++23 } }
// Test from [class.copy.elision]/4.

class Thing {
public:
  Thing();
  ~Thing();
  Thing(Thing&&);
private:
  Thing(const Thing&);
};

Thing f(bool b) {
  Thing t;
  if (b)
    throw t;            // OK, Thing(Thing&&) used (or elided) to throw t
  return t;             // OK, Thing(Thing&&) used (or elided) to return t
}

Thing t2 = f(false);    // OK, no extra copy/move performed, t2 constructed by call to f

struct Weird {
  Weird();
  Weird(Weird&);
};

Weird g(bool b) {
  static Weird w1;
  Weird w2;
  if (b) {
    return w1;  // OK: Weird(Weird&)
  } else {
    return w2;  // { dg-error "cannot bind non-const lvalue reference" }
  }
}

int& h(bool b, int i) {
  static int s;
  if (b)
    return s;   // OK
  else
    return i;   // { dg-error "cannot bind non-const lvalue reference" }
}

decltype(auto) h2(Thing t) {
  return t;     // OK, t is an xvalue and h2's return type is Thing
}

decltype(auto) h3(Thing t) {
  // OK, (t) is an xvalue and h3's return type is Thing&&
  return (t); // { dg-warning "reference to local variable" }
}
