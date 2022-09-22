/* { dg-do compile } */

int a[8], b[8];

int f1()
{
  a[0] = b[4] + 1;
  a[1] = b[5] + 1;
  a[2] = b[6] + 1;
  a[3] = b[7] + 1;
  a[4] = b[0] + 1;
  a[5] = b[1] + 1;
  a[6] = b[2] + 1;
  a[7] = b[3] + 1;
}

unsigned short c[2], d[2];
void f2() {
  c[0] += d[1];
  c[1] += d[0];
}

typedef int v4si __attribute__((vector_size(16)));
void f3(v4si x) {
  a[0] = b[1] + x[1];
  a[1] = b[0] + x[3];
}
