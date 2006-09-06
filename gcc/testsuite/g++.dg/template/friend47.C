// PR c++/19809

template<int i>
struct n{
  friend void foo(){ }		// { dg-error "defin" }
};

int main(){
  n<1> n1;
  n<2> n2;
}
