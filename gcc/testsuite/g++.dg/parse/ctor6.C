// { dg-do compile }

// There is no ambiguity in finding a right constructor for X b(a).

class X {
public:
  X(const X&, int = 3);
};

extern X a;
X b(a);
