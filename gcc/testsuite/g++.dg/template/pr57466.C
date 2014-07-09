// DR 1584, PR c++/57466

template<class T> void f2(const T*);
void g2();

void m() {
  f2(g2);    // OK: cv-qualification of deduced function type ignored
}
