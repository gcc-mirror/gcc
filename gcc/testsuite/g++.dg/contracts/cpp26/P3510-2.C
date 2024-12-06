// { dg-do compile }
// { dg-options "-fcontracts -fcontracts-nonattr -std=c++23" }

struct S{
  bool f();
  bool k;
  S() pre(&k); // { dg-error "required when accessing a member" }
  ~S()post(&f());  // { dg-error "required when accessing a member" }
  S(int i) pre(&(this->k))
      pre([](){
            struct SIn{
              bool i = true;
              bool f()  const pre(i) { return i;}
              SIn() pre(&i){}; // { dg-error "required when accessing a member" }
              ;
            };
            SIn sin;
            return sin.f();
  }());
};
int main()
{
}
