// N5008 :
// [over.call.func/p3.1
// if the unqualified function call appears in a precondition assertion of a constructor or a postcondition
// assertion of a destructor and overload resolution selects a non-static member function, the call is
// ill-formed;
// { dg-do compile { target c++23 } }
// { dg-additional-options "-fcontracts" }

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

struct S2{
  bool f() const { return true;}
  bool i = true;
  bool k = true;
  S2() pre(&(this->k) != &(this->i)) post(i) {};
  ~S2() pre(f()){};
};
int main()
{
}
