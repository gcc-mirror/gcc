// PR middle-end/80707 ICE: extra outgoing edge causes verify_flow_info error.
// { dg-do compile }
// { dg-options "-O3" } */

struct A {
  int m_fn1(int &) const;
};
int A::m_fn1(int &p1) const {
  int a[6];
  int b = 0;
  for (int i;; i++) {
    if (a[i])
      break;
    b++;
  }
  while (b) {
    int c;
    switch (b) {
    case 1:
      c = 0;
      break;
    case 5:
      c = a[0];
    }
    if (c)
      p1 = 0;
    b--;
  }

  return 0;
}
