// { dg-do compile { target c++17_only } }
// { dg-options "-fconcepts" }

template<class T>
concept bool Addable(){
 return requires(T x){
  {x + x} -> T;
 };
}

int main(){
 Addable t = 0;
}
