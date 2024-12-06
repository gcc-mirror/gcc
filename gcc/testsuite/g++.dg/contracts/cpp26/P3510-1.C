// { dg-do run }
// { dg-options "-fcontracts -fcontracts-nonattr -std=c++23" }

struct S{
  bool f() const { return true;}
  bool i = true;
  bool k = true;
  S() pre(&(this->k) != &(this->i)) post(i) {};
  ~S() pre(f()){};
};
int main()
{
  S s;

}
