
void
a() {
  short *b;
  short c;
  long long *d = a;
  for (;;) {
    long long *e = a;
    (*d *= *e - c) / *b ?: (*b = 0);
  }
}

