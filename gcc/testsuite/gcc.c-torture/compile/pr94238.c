enum { false, true } a;
int b, c, d, e, f;
int fn3();
void fn2();

void fn1() {
  _Bool g, h = false, i = false;
  int j;
  c = b && f || d;
  if (c) {
    if (d)
      i = true;
    _Bool k = b;
    int l = e, m = a;
    g = k && l < m || l > m;
  }
  if (g)
    h = true;
  if (i)
    fn2();
  h &&j &&fn3();
}
