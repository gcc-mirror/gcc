// { dg-do run  }
// PRMS Id: 1502
// Bug: g++ fails to resolve 'gnc' in the call to 'grid'.

template<class T> class foo { 
public:
  foo() { } 
};

template<class T> class bar : public foo<T> {
public:
  bar() : foo<T>() {}
};

template<class T> class ben : public foo<T> {
public:
  ben() : foo<T>() {}
  void grid(T (*f)(bar<T>&),bar<T>& x,bar<T>& y,bar<T>& param);
};

template<class T> void ben<T>::grid(T (*f)(bar<T>&),bar<T>& x,bar<T>& y,bar<T>& param) { }

template<class T> T gnc(bar<T>& a)
{
  return 0;
}

int main()
{
  ben<double> a;
  bar<double> x,y,p;
  a.grid(gnc,x,y,p);
  return 0;
}
