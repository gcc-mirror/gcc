union a {
  float _Complex b;
  long long c;
};

void g(union a);

void e() {
  union a f = {1.0f};
  g(f);
}
