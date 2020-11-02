/* { dg-do compile } */
/* { dg-require-effective-target c++17 } */

struct u {
  int b;
  int c;
  template <typename d, typename e> u(d, e);
};
template <class, class> struct f { u g; };
template <class h, class i> class v {
  typedef f<h, i> k;
  k *l[4];
  k m;
public:
  v(h, h);
  void aa(h, i);
};
template <class h, class i> void v<h, i>::aa(h, i) { n(&l[1], &m); }
template <class h, class i> void n(f<h, i> **o, f<h, i> *ab) {
  bool p, r;
  f q = **o;
  f<h, i> *t;
  h a = q.g;
  h b = t->g;
  if (r)
    ;
  else
    goto ac;
s:
  p = a.b || a.c < b.c;
  if (p)
    goto s;
ac:
  ab->g = b;
  b = t->g;
  goto s;
}
template <class, class, class> class w {};
template <class> class x;
template <class, class> class z;
class ad {
public:
  template <typename, typename y, typename ae, typename af, typename ag>
  static void ah(const z<y, ae> &, const z<y, af> &, x<ag> *&);
};
template <typename, typename y, typename ae, typename af, typename ag>
void ad::ah(const z<y, ae> &ai, const z<y, af> &aj, x<ag> *&) {
  u c(0, 0), d(0, 0), g(aj, ai);
  v<u, y> e(c, d);
  e.aa(g, 0);
}
template <class, class> class ak;
template <class, class, class al, class am, class an>
void ao(ak<al, am> ap, ak<al, an> aq) {
  x<double> *f;
  ad::ah<int>(*ap.ar, *aq.ar, f);
}
template <typename, typename, typename al, typename am, typename an,
          typename as, typename at>
void au(w<al, am, as> ap, w<al, an, at> aq) {
  ao<int, double>(static_cast<as &>(ap), static_cast<at &>(aq));
}
template <class, class> class z {};
template <class, class> class ak : public w<int, int, ak<int, int>> {
public:
  z<int, int> *ar;
};
template <class, class, class> class av;
template <typename, typename, typename, typename al, typename am, typename an,
          typename aw, typename ax>
void ay(av<al, am, aw>, av<al, an, ax>) {
  aw h, i;
  au<int, double>(h, i);
}
template <class, class, class> class av {};
class az {
public:
  typedef av<int, double, ak<int, double>> ba;
};
int main() {
  az::ba j, k;
  ay<int, double, az>(j, k);
}
