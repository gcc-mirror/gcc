// PR c++/60967
// { dg-require-effective-target c++11 }
// { dg-options "-fcilkplus" }

int container[] = {};
template <class foo>
void bar() {
  for (int &v : container) { }
}
