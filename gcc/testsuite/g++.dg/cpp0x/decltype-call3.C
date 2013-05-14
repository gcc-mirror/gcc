// Testcase for N3276 and operator overloading
// { dg-require-effective-target c++11 }

struct A;
struct B {
  A operator()(int);
  A operator[](int);
  A operator=(int);
  A operator+=(int);
  A operator-=(int);
  A operator*=(int);
  A operator/=(int);
  A operator^=(int);
  A operator&=(int);
  A operator|=(int);
  A operator<<=(int);
  A operator>>=(int);
};

A operator-(B);
A operator+(B);
A operator*(B);
A operator&(B);
A operator!(B);
A operator~(B);
A operator++(B);
A operator--(B);

A operator+(B,B);
A operator-(B,B);
A operator*(B,B);
A operator/(B,B);
A operator%(B,B);
A operator^(B,B);
A operator&(B,B);
A operator|(B,B);
A operator<(B,B);
A operator>(B,B);
A operator,(B,B);
A operator<<(B,B);
A operator>>(B,B);
A operator==(B,B);
A operator->*(B,B);

#define TRY(E) static_cast<decltype(E)*>(0)

template <class B>
void f()
{
  B b;
  TRY(b(0));
  TRY(b[0]);
  TRY(b=0);
  TRY(b+=0);
  TRY(b-=0);
  TRY(b*=0);
  TRY(b/=0);
  TRY(b^=0);
  TRY(b&=0);
  TRY(b|=0);
  TRY(b<<=0);
  TRY(b>>=0);

  TRY(-b);
  TRY(+b);
  TRY(*b);
  TRY(&b);
  TRY(!b);
  TRY(~b);
  TRY(++b);
  TRY(--b);

  TRY(b+b);
  TRY(b-b);
  TRY(b*b);
  TRY(b/b);
  TRY(b%b);
  TRY(b^b);
  TRY(b&b);
  TRY(b|b);
  TRY(b>b);
  TRY(b<b);
  TRY((b,b));
  TRY(b<<b);
  TRY(b>>b);
  TRY(b==b);
  TRY(b->*b);
}

int main()
{
  B b;
  TRY(b(0));
  TRY(b[0]);
  TRY(b=0);
  TRY(b+=0);
  TRY(b-=0);
  TRY(b*=0);
  TRY(b/=0);
  TRY(b^=0);
  TRY(b&=0);
  TRY(b|=0);
  TRY(b<<=0);
  TRY(b>>=0);

  TRY(-b);
  TRY(+b);
  TRY(*b);
  TRY(&b);
  TRY(!b);
  TRY(~b);
  TRY(++b);
  TRY(--b);

  TRY(b+b);
  TRY(b-b);
  TRY(b*b);
  TRY(b/b);
  TRY(b%b);
  TRY(b^b);
  TRY(b&b);
  TRY(b|b);
  TRY(b>b);
  TRY(b<b);
  TRY((b,b));
  TRY(b<<b);
  TRY(b>>b);
  TRY(b==b);
  TRY(b->*b);

  f<B>();
}
