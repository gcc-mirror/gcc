template<class Type>
class A
{
public:
  Type m;
};

template<class Type>
void f(A<Type>& a, Type d)
{
  A.m=d; // ERROR - invalid use of template
}

int main()
{
  A<int> a;
  f(a,2);
}
 
