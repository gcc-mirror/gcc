// PR c++/68374
// { dg-options "-Wshadow" }

class f {
  static int mVar;  // { dg-message "shadowed declaration" }
  int g(int x) { int mVar=3; return x+mVar; }  // { dg-warning "shadows a member of 'f'" }
};
int f::mVar = 1;
