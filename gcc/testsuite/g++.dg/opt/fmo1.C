// PR rtl-optimization/115038
// Reported by Christoph Reiter <reiter.christoph@gmail.com>

// { dg-do compile }
// { dg-options "-O2 -fno-omit-frame-pointer" }

struct d {
  d();
};

struct e {
  e() : c(1.0) {}
  float c;
};

class k {
  d g;
  e h;
};

class a {
  k f;
} a;

k b;
