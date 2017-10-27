// { dg-options "-std=c++17 -fconcepts" }

template<class T>
concept bool Addable(){
 return requires(T x){
  {x + x} -> T;
 };
}

int main(){
 Addable t = 0;
}
