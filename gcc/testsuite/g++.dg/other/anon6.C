// PR c++/60353

struct A {
  A(int);
};
typedef struct {
  A format;
} B;
