// PR c++/20734

struct A;
void blah(int A::*);
struct A{
  int a;
};
template<typename T>
void hoho(){
  blah(&A::a);
}
