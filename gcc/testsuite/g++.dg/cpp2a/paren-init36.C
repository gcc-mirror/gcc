// PR c++/77841
// { dg-do compile { target c++20 } }

int *p0 = new int[1]();
int *p1 = new int[1](1);
int *p2 = new int[4](1, 2, 3, 4);
int *p3 = new int[2](1, 2, 3, 4); // { dg-error "too many initializers" }

char *c1 = new char[]("foo");
char *c2 = new char[4]("foo");
char *c3 = new char[]{"foo"};
char *c4 = new char[4]{"foo"};
char *c5 = new char[3]("so_sad"); // { dg-error "too long" }
char *c6 = new char[3]{"so_sad"}; // { dg-error "too long" }
