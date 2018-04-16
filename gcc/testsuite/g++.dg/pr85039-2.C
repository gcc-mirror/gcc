// { dg-do compile }

struct d {
  static d *b;
} * d::b(__builtin_offsetof(struct { // { dg-error "types may not be defined" }
  int i;
  struct a { // { dg-error "types may not be defined" }
    int c() { return .1f; }
  };
}, i));
