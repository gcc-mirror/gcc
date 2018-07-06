// PR c++/84421
// { dg-options -std=c++17 }

struct A{
  constexpr operator bool() const { return true; }
};

int main(){
  auto f = [](auto v){
    if constexpr(v){}
  };
  A a;
  f(a);
}
