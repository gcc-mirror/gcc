// PR c++/107575

void f (void) {
  virtual int f (void) const; // { dg-line line_4 }
  virtual int f (void); // { dg-line line_5 }
}

// { dg-error "outside class declaration" {} { target *-*-* } line_4 }
// { dg-error "cannot have cv-qualifier" {} { target *-*-* } line_4 }
// { dg-error "ambiguating new declaration of" {} { target *-*-* } line_4 }
// { dg-error "outside class declaration" {} { target *-*-* } line_5 }
