// { dg-do run  }
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

int main() {
  return &d.i == &d.c;
}
