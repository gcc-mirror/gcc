typedef a;
b(void *c, a d) {
  if (c)
    e(0, __PRETTY_FUNCTION__);
}
typedef f, g;
__attribute__((optimize(0))) h() {
  g i;
  b(i, sizeof(f));
}

