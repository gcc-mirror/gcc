// PR c++/53484
// { dg-do compile { target c++11 } }

template<class T,class U> struct ST;
template<class T> struct ST<T,T> {};

template <class T>
void f(T x){
   [&]{
     auto y = x;
     ST<decltype(y),int>();
   }();
}

int main(){ f(0); }
