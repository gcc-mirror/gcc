// PR c++/13907

struct A {
  operator int & ();
  operator const int & () const;
};
                                                                               
void f(int &);
void f(const int &);
          
int main() {
  const A x = A();
  f(x);
}
