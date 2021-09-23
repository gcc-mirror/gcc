int a;
short b, c;
short d(short e, short f) { return e + f; }
void g(void) {
  a = -9;
  for (; a != 51; a = d(a, 5))
    b |= c;
}
