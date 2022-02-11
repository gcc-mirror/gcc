/* { dg-do compile }  */
/* { dg-options "-O2" }  */

struct b1 {
      char c;
      char bg;
};

struct bj1 {
  char bk;
  struct b1 bn[];
};

struct b2 {
      short c;
      char bg;
};

struct bj2 {
  short bk;
  struct b2 bn[];
};

struct b3 {
      int c;
      char bg;
};

struct bj3 {
  int bk;
  struct b3 bn[];
};


struct bj1 at1;
struct bj2 at2;
struct bj3 at3;

int bu;
void a();

void f() {
  a(at1.bn[bu]);
  a(at2.bn[bu]);
  a(at3.bn[bu]);
}

/* { dg-final { scan-assembler "@at1\\+1" } } */
/* { dg-final { scan-assembler "@at2\\+2" } } */
/* { dg-final { scan-assembler "add3" } } */
