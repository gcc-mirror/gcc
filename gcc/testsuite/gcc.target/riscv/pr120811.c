/* { dg-do compile } */
/* { dg-options "-O2 -march=rv64gc -mabi=lp64d -fdump-rtl-reload -fdump-rtl-late_combine2" { target { rv64 } } } */
/* { dg-options "-O2 -march=rv32gc -mabi=ilp32d -fdump-rtl-reload -fdump-rtl-late_combine2" { target { rv32 } } } */
/* { dg-skip-if "" { *-*-* } "-fomit-frame-pointer" } */

double *a, *b, *ab, *ac, *ad, *ae, *af, *c, *i, *k, *l;
int n(int *, int, int, int), d, ag, aj;
long ai, e;
double f, g, h, o, p;
int m;
void q() {
  long am = n(&d, 0, 1, 0);
  for (;;)
    for (int j = ag; j; ++j) {
      int ao = 0;
      for (; ao < aj; ao++) {
        long ap = ao + j;
        double ar = ad[ap] = ae[ap], az;
        switch (m)
        case 4: {
          o = (&l[ap])[2] - (&l[ap])[3] * ((char *)ap)[am] + (&l[ap])[e] +
              (&l[ap])[am];
          p = (&l[ap])[ai - 1] + (&ap)[ai] * (&l[ap])[am * ai] +
              (&l[ap])[ai * 3];
          az = (&b[ap])[1] +
               (&b[ap])[3] * i[ap] * (&ap)[203] * ((char *)&i[ap])[ai] -
               (&i[ap])[ai * 3];
          g = (&i[ap])[e] + (&i[ap])[ai];
          h = (&ab[ap])[am] * (&ab[ap])[2] - ((char *)ap)[ai] +
              (&ab[ap])[ai] * (&ap)[0] * (&af[ap])[am] * (&af[ap])[e] +
              (&ap)[2] + (&af[ap])[e * am] + (&af[ap])[am * 3];
        }
          ar = az * f * k[ap];
        c[ap] = ar;
        a[ap] = ac[ap];
      }
    }
}
/* { dg-final { scan-rtl-dump-not "const_sum_of_two_s12" "reload" } } */
/* { dg-final { scan-rtl-dump-not "const_sum_of_two_s12" "late_combine2" } } */
/* { dg-final { scan-assembler "addi.*\[ats\]\[0-9\]*,sp,\[0-9\]*\n\tld\t.*,2047\(.*\).*" } } */
