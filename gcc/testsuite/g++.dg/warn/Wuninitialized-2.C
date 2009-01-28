/* { dg-do compile } */
/* { dg-options "-O -Wuninitialized" } */

struct S8 { template<typename T> S8(T) { } };

template<typename T> struct S10;
template<typename T> struct S10<T()> { typedef T S12; typedef S8 S1(); };

template<typename T> struct S3 { };
template<typename T> struct S11 { S11(S3<T>); };

struct S2
{
  template<typename T> operator S11<T>() { return S11<T>(S5<T>()); }
  template<typename T> struct S5:public S3<T>
  {
    virtual typename S10<T>::S12 S13() {
      return 0;
    }
  };
};

template<typename T> S11<T> S6(S3<T>) { return S11<T>(S3<T>()); }
template<typename S12> struct S7 { typedef S12 S15(); };

struct S4
{
  template<typename T> operator S11<T>()
  {
    struct S14:public S3<T>
    { 
      S14(S2 x):S11_(x) { }
      S11<typename S7<typename S10<T>::S12>::S15> S11_;
    };
    return S6(S14(S11_));
  }
  S2 S11_;
};

struct S9
{
  template<typename F> operator S11<F>() { return S11<F>(S14<F>(S11_)); }
  template<typename F> struct S14:public S3<F>
  {
    S14(S4 x):S11_(x) { }
    S11<typename S10<F>::S1> S11_;
  };
  S4 S11_;
};

void S15(S11<void()>);
void S16() { S9 x; S15(x); }

