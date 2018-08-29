// PR c++/80026
// { dg-do compile { target c++11 } }

void g(int) {}
void g(bool){}

template <class...S>
void f(S...){}

int main(){
  f(&g);  // { dg-error "too many arguments" }
}
