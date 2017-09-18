// { dg-options -std=c++17 }

template<class T> struct S{S(T){}};

int main() {
  S{1};
} 
