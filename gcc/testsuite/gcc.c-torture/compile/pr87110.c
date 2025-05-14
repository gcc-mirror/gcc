enum a { b, c };
struct d {
  _Bool e;
  enum a f;
};
int g, h;
void
i(void) {
  struct d j[h];
  j[0] = (struct d){.f = c};
  for (; g;)
    (struct d){};
}

