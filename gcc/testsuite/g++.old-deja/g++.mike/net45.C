// { dg-do assemble  }

template <class T1, class T2>
struct pair {
    T1 first;
    T2 second;
    pair(const T1& a, const T2& b) : first(a), second(b) {}
};

struct myint {
  myint() {
  }
  myint(const myint& mi) {
  }
  myint& operator=(const myint& mi) { return *this; }
};

extern pair<const myint, myint> a;
pair<const myint, myint> b(a);
