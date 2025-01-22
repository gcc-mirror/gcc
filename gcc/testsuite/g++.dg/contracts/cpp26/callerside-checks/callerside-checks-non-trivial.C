// { dg-do compile }
// { dg-options "-std=c++2a -fcontracts -fcontracts-nonattr -fcontract-evaluation-semantic=observe -fcontracts-nonattr-client-check=all" }


struct S{
  S(){};
  S(const S&){}
  ~S(){};
  int x = 0;
};

void f(S s) pre(s.x == 1 ) {};

int main()
{
  S s;
  f(s);
}
