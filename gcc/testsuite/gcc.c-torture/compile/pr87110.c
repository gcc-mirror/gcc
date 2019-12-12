// { dg-require-effective-target alloca }
enum a { b, c };
struct d {
  _Bool e;
  enum a f
};
g, h;
i() {
  struct d j[h];
  j[0] = (struct d){.f = c};
  for (; g;)
    (struct d){};
}

