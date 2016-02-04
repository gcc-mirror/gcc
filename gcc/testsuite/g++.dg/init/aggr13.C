// PR c++/67550
// { dg-do run }

struct S {
  int x;
  int y;
};
int foo() { return 1; }

int main() {
  S const data[] = {{0, foo()}};

  S data2[] = {data[0]};

  if (!data2[0].y)
    __builtin_abort();
}
