// { dg-options "-std=c++26 -fcontracts -fcontracts-nonattr" }

struct F {
  int a;
  F (int b)
    pre (b >= 0)
    post (this->a > 10);

  ~F () = default;

  int swap (const int b)
    pre (b > 5)
    pre (a != 0)
    post (a == b) ;
};

F::F (int b)
  pre (b >= 0)
  post (this->a > 10)
  : a (b)
{}

int
F::swap (const int b)
    pre (b > 5)
    pre (a != 0)
    post (a == b)
{ int x = a; a = b; return x; }

#if 0
int main ()
{
  F f{0};
  f.swap (11);
}
#endif
