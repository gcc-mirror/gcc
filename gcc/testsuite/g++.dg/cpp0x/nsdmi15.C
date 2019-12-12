// PR c++/89488
// { dg-do compile { target c++11 } }

struct zl {
  struct {
    int x2 = zl ();  // { dg-error "default member" }
  } fx;
};
