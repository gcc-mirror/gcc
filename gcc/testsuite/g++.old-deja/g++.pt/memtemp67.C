// { dg-do run  }
template <class T>
struct A
{
  template <class T2>
  operator A<T2>() const { return A<T2>(); }
};

int main()
{
  A<int> a1;
  A<long> a2;    
  A<double> a3;
  A<char> a4;

  a2 = a1.operator A<long>();
  a3 = (A<double>) a1;                   
  a4 = a1;
}
 
