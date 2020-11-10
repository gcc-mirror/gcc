// { dg-do compile }
// { dg-options "-fno-guess-branch-probability -fno-tree-pta -O1" }

void *b, *c;
struct H {
  virtual bool accept(const char *, unsigned long, int *, bool);
};
char accept_bt[1], accept_cd[1];
int accept_cb;
bool accept_cb_0;
class t : H {
  bool accept(const char *, unsigned long bd, int *bg, bool) {
    long bu = sizeof(int) + bd;
    char *bw = bu > sizeof(accept_bt) ? new char : accept_bt,
         *cf = bd ? new char : accept_cd;
    __builtin___memcpy_chk(b, c, bd, 0);
    if (bw != accept_bt)
      delete bw;
    bool ci = cj((int *)cf, bg), atran = bp && accept_cb_0;
    atran &&ci &&cm(&accept_cb);
    return ci;
  }
  bool cj(int *, int *);
  bool cm(int *);
  bool bp;
};
void bj() { new t; }
