// PRMS Id: 11420
// Bug: Can't handle indirect virtual template base init.

extern "C" int printf (const char *, ...);

template<class T>
class Vbase {
 public:
  Vbase(T i) { printf ("%d\n", i); }
};

template<class T>
class D1 : virtual public Vbase<T> {
 public:
  D1(T i) : Vbase<T>(i) {}
};

template<class T>
class D2 : virtual public Vbase<T> {
 public:
  D2(T i) : Vbase<T>(i) {}
};

template<class T>
class Most : public D1<T>, public D2<T> {
 public:
  Most(T i) : D1<T>(i), D2<T>(i), Vbase<T>(i) {}
};

int main () {
  Most<int> x(2);
  return 0;
}
