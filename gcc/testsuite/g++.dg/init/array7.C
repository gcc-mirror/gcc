struct S {
  virtual void v () {}
  void f (const float g[3]);
  float h[3];
};

void g () {
  S s1, s2;
  s1 = s2;
}

void S::f (const float g[3]) {}



