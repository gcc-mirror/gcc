//Check whether namespace-scoped template instantiations
//are mangled differently.

namespace X{
  template<class T>
  struct Y{
   int f(T){
     return 1;
   }
   template<class X>void g(){}
  };
}

template<class T>
struct Y{
  int f(T){
    return 2;
  }
};

int main()
{
  X::Y<int> z;
  if (z.f(4) != 1)
    return 1;
  z.template g<long>();

  Y<int> z1;
  if (z1.f(5) != 2)
    return 1;
  return 0;
}

