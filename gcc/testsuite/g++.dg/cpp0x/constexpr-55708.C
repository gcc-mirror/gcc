// PR c++/55708
// { dg-do compile { target c++11 } }

template<int N,int NNN>
struct AA { static constexpr int val = N; };

template<typename A,typename B>
//constexpr unsigned long long mymax(A a,B b){ // <-- compiles 
constexpr unsigned long long mymax(A && a,const B& b){
  return a<b?b:a;
}

template<char... List>
constexpr long long operator"" _y() noexcept
{
  return AA<1, mymax(1,2)>::val; // <-- crashes gcc
  // return mymax(1,2);   // <-- compiles
  // return AA<1,2>::val; // <-- compiles
}

template<char... List>
constexpr unsigned long long do_y() noexcept
{
  return AA<1, mymax(1,2)>::val; // <-- crashes gcc
}

int main()
{
  return 1_y + do_y();
}
