// prms-id: 8804

extern "C" int printf (const char *, ...);

struct Fails {
  int i;
  union {
    union {
      int c;
    };
  };
};

Fails d;

main() {
  return &d.i == &d.c;
}
