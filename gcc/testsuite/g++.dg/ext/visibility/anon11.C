// PR c++/55877
// { dg-final { scan-assembler-not "\\.local" } }

typedef struct {
  typedef enum { X, Y } A;
  typedef struct { } B;
  struct C { };
} D;

D d;
D::A a;
D::B b;
D::C c;
