// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }

template<class T>
concept Addable =
 requires(T x){
  {x + x} -> T;	// { dg-error "return-type-requirement is not a type-constraint" }
 };

int main(){
 Addable auto t = 0;
}
