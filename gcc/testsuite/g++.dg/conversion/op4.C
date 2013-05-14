/* PR c++/26698 */
/* { dg-do compile } */

struct X {
  int x;
  X (int i = 0) : x (i) {}
  operator X& (void) const {
    return *(new X);
  }
};

void add_one (X & ref) { /* { dg-message "in passing argument" } */
  ++ ref.x;
}

void foo() {
  X const a (2);
  add_one(a); /* { dg-error "invalid initialization of reference of type" } */
}
