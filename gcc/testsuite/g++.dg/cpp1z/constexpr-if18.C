// PR c++/85200
// { dg-do compile { target c++17 } }

template <typename T>
void f(){
  [](auto v, auto b){
    if constexpr (sizeof(v) == sizeof(int)) {
	auto x = b;
      }
  }(0, 1);
}

int main(){
  f<int>();
}
