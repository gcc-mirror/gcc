int printf (const char *, ...);

int a;
static void b(int c) {
  if (c)
    printf("%d", a);
}
void e() {
  int d = 0;
  b(d);
}
