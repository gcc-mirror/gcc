class a {
  int b;
  void *c;

public:
  bool aa();
  int &ab() {
    if (aa()) {
      void *d(c);
      return static_cast<int *>(d)[b];
    }
    return *(int *)0;
  }
};
typedef enum {E} e;
class f : public a {
  int g;

public:
  int ac() {
    if (g)
      return 1;
    return ac();
  }
};
int *ad;
struct h {
  static int ae(e, int *m) {
    f ag;
    int *ah;
    while (!0) {
      ad = &ag.ab();
      ah = ad + ag.ac();
      while (ad < ah)
        *m = *ad++;
    }
  }
};
template <class, class>
void i(int *, int *, int, int *, e n, int *o) {
  h::ae(n, o);
}
int aq, ar, as, at, au;
void aw() { i<int, bool>(&aq, &ar, as, &at, (e)0, &au); }

