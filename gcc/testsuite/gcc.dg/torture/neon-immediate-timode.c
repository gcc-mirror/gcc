union a {
  char b;
  long long c;
};
union a d;
int g(int, union a, union a);
void e() {
  union a f[2] = {-1L};
  g(0, d, f[0]);
}
