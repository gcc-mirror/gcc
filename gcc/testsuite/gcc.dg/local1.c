static int i;

extern int i;

static void f() {
  extern int i;
}
