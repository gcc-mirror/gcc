template <class X> class TC {
public:
  X aaa;
  static X sss;
  TC(X a) {aaa = a; }
  TC(X a, X s) {aaa = a; sss = s; }
  void sz(X s) { sss = s; }
  void syy(X syarg) { sss = syarg; }
};

long TC<long>::sss;
float TC<float>::sss;

TC<long> xjj(1,2);

int main(int,char*) {
  TC<float> xff(9.9,3.14);
  xjj.sz(123);
  xff.sz(2.71828);
  return 0;
}
