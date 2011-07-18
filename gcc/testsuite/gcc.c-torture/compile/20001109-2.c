extern struct foo bar;
struct foo {
  int a;
};

int tst[__alignof__ (bar) >= __alignof__ (int) ? 1 : -1];
