// { dg-do run }
// { dg-options "-fabi-version=0" }

struct E1 {};
struct E2 : public E1 {};
struct S1 { int i; };
struct S2 : public S1, E2 {};

S2 s2;

int main () {
  if ((char *)(E2*) &s2 != (char *)&s2)
    return 1;
}
