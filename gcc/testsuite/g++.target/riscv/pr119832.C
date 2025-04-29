/* { dg-do compile } */
/* { dg-options "-O2 -march=rv64gcv -mabi=lp64 -ffast-math" } */

struct ac  {
  ~ac();
  void u();
};
struct ae {
  int s;
  float *ag;
};

float c;

void ak(ae *al, int n) {
  ac d;
  for (int i;i<n;++i) {
    float a = 0;
    for (long j; j < al[i].s; j++)
      a += al[i].ag[j];
    c = a;
    d.u();
  }
}

/* { dg-final { scan-assembler-not "frrm\t" } } */
/* { dg-final { scan-assembler-not "fsrm\t" } } */
