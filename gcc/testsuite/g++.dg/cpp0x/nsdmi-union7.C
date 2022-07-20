// PR c++/94823
// { dg-do compile { target c++11 } }

struct A{
  A(){}
};
union C{
  A a;
  int b = 0;
};
int main(){
  C c;
}
