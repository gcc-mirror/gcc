// { dg-do run }

// Avoid -pedantic-error default
// { dg-options "" }

// Make sure the functions referenced by various forms of
// address-taking are marked as used and compiled in.

static void ac() {}
void a() {
  ac[0](); // { dg-warning "arithmetic" }
}

static void bc() {}
void b() {
  (&*&*&*&bc)();
}

template <typename U> U cc() {}
void (*c())() {
  return cc;
}

template <typename T>
struct x {
  void a(int);
  template <typename U> static U a(x*) {}
  static void a(long) {}
  static void a(void *) {}
  static void a() {
    void (*p0)(void*) = x().a;
    p0(0);
    void (*p1)(long) = a;
    p1(0);
    void (*p2)() = a;
    p2();
    void (*p3)(x*) = a;
    p3(0);
  }
};

struct z {
  void a(int);
  template <typename U> static U a(z*) {}
  static void a(long) {}
  static void a(void *) {}
  static void a() {
    void (*p0)(void*) = z().a;
    p0(0);
    void (*p1)(long) = a;
    p1(0);
    void (*p2)() = a;
    p2();
    void (*p3)(z*) = a;
    p3(0);
  }
};

int main(int argc, char *argv[]) {
  if (argc > 1) {
    x<void>().a();
    z().a();
  }
}
