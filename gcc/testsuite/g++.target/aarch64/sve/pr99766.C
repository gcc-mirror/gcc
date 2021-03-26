/* { dg-do compile } */
/* { dg-additional-options "-O3 -march=armv8.2-a+sve" } */
typedef float a __attribute__((__mode__(HF)));
typedef struct {
  a b;
  a c;
} d;
int e;
d *f, *g;
__fp16 h;
void j() {
  for (int i;; ++i) {
    auto l = &f[i];
    for (int k; k < e;) {
      k = 0;
      for (; k < e; ++k)
        g[k].b = l[k].b * l[k].c;
    }
    for (int k; k < e; ++k) {
      g[k].b *= h;
      g[k].c *= h;
    }
  }
}
