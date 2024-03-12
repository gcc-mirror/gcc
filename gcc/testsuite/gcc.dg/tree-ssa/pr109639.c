// { dg-do compile }
// { dg-options "-O2" }

extern int k[];
int m;
int* j();
void f(int *howto) {
  short __trans_tmp_1;
  long offset = howto - k;
  __trans_tmp_1 = offset;
  for (;;) {
    if (howto == 0)
      return;
    if (__trans_tmp_1) {
      howto = j();
      m = *howto;
    }
    f(howto);
  }
}
