// { dg-do compile }
// { dg-options "" }

struct A {
  union {
    int a;
    char b;
  };
};

struct A x = {
  .a = 5,
};
