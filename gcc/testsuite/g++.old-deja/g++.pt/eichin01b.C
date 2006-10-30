// { dg-do run  }
template <class X> class TC {
public:
  X aaa;
  static X sss;
  TC(X a) {aaa = a; }
  TC(X a, X s) {aaa = a; sss = s; }
  void sz(X s) { sss = s; }
  void syy(X syarg) { sss = syarg; }
};

template <> long TC<long>::sss = 0;
template <> float TC<float>::sss = 0.0;

TC<long> xjj(1,2);

int main(int,char**) {
  TC<float> xff(9.9,3.14);
  xjj.sz(123);
  xff.sz(2.71828);
  return 0;
}
