// Build don't link:

union Un {int i;};

template<class T1, class T2> struct St1 {};
template<class T> struct St1<Un,T> {};

template<class T> struct St2 {};
template<> struct St2<Un> {};

template<class T1, class T2> struct St3 {};
template<> struct St3<Un,int> {};

void f() {
  St1<int,int> s1;
  St2<int>     s2;
  St3<int,int> s3;
}
