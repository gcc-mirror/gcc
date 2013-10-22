// PR c++/46420
// { dg-options -std=c++11 }

template<typename> class vector { };
struct A{};
template <class T1>
void complete_test(vector<T1> data1){
        A drop=A();
}
int main(){
  vector<double> vect1;
  complete_test(vect1);
}
