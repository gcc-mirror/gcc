// PR c++/84036
// { dg-do compile { target c++14 } }

template < typename... T > void sink(T ...){}

template < typename T >
auto f(T){
  auto l = [](auto ... i){
    [i ...]{
      sink(i...);
      [=]{ sink(i ...); }();
    }();
  };
  l();
  l(42);
  l(0.1,0.2);
}

int main(){
    f(0);
}
