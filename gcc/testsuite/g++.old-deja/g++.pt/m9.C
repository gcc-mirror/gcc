// Build don't link: 

struct A { A() { a = 1; } int a; };
struct Q {
  struct A { A() { a = 2; } int a; };
  struct R {
     struct A { A() { a = 3; } int a; };
     A aaz;
  };
  R rrr;
  A aay;
}
;

Q qqq;
A aav;
