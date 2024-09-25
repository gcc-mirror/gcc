/* { dg-do compile } */
/* { dg-additional-options "-O3" } */

int m[40];

template <typename k> struct j {
  int length;
  k *e;
  void operator[](int) {
    if (length)
      __builtin___memcpy_chk(m, m+3, sizeof (k), -1);
  }
};

j<j<int>> o;

int *q;

void ao(int i) {
  for (; i > 0; i--) {
    o[1];
    *q = 1;
  }
}
