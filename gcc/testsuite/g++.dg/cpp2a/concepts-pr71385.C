// { dg-do compile { target c++20 } }

template<class From, class To>
concept convertible_to = requires(From (&f)(), void (&g)(To)) { g(f()); };

template<class T>
concept Addable =
 requires(T x){
  {x + x} -> convertible_to<T>;
 };

int main(){
 Addable auto t = 0;
}
