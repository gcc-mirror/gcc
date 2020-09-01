// PR c++/77841
// { dg-do compile { target c++11 } }

char *p1 = new char[4]{"foo"};
char *p2 = new char[5]{"foo"};
char *p3 = new char[3]{"foo"}; // { dg-error "too long" }
