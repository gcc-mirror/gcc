// { dg-options -std=c++1z }

template<class T> struct S{S(T){}};

int main() {
  S{1};
} 
