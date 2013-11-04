// PR c++/52071

struct C1 {
    C1(int);
};

struct C2 {
    C2();
    C2(C1);
};

void f()
{
  int x;
  int y = 1;
  C2  vc;

  vc = C2(C1(x = y));

  vc = (C2(C1(x = y)));

  vc = (C2(C1((0, x = y))));
}
