// { dg-do compile { target c++20 } }
// { dg-additional-options "-fcontracts -fcontract-evaluation-semantic=observe -fcontracts-client-check=all" }


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
