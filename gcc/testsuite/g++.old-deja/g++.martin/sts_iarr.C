// egcs-bugs 999-02-22 14:26 Stefan Schwarzer
// sts@ica1.uni-stuttgart.de
// should compile and return 0  

template <int N>
struct Outer{
  struct Inner{
    Inner(int n): sum(n){}

    typename Outer<N-1>::Inner operator[](int n) const
    { return typename Outer<N-1>::Inner(sum + n); }

    int sum;
  };

  typename Outer<N-1>::Inner operator[](int n) const
  { return typename Outer<N-1>::Inner(n); }
};


// specializations for N==1
template<>
struct Outer<1> { 
  struct Inner {
    Inner(int n): sum(n){}

    int operator[](int n) const 
    { return sum+n; }
    
    int sum;
  };

  int operator[](int n) const
  { return n; }
};  


int main()
{
  Outer<1>  sum1;
  //std::cout << sum1[1] << "\n";
  Outer<2>  sum2;
  //std::cout << sum2[1][1] << "\n";
  return sum1[1] + sum2[1][1] - 3;
}
