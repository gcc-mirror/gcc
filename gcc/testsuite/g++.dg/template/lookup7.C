class S;

template<class T>
int f(T, S);

class S {
  template<class T>
  friend int f(T t, S) { t; return 0; }
};
