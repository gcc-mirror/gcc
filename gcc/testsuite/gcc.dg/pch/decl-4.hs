typedef struct foo {
  int a;
  char b;
  struct foo *c;
} foo_s;
typedef struct foo *foo_p;
extern foo_p foop;
