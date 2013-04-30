// Core 1609
// { dg-require-effective-target c++11 }

template<typename... T>
void f2(int a = 0, T... b, int c = 1);

int main(){
  f2<>(); // parameter a has the value 0 and parameter c has the value 1
}
