template<class T_type, int N>
class foo {
public:
  enum bar { z = 0 };
};

template<int N>
class foo<double, N> {
public:
  enum bar { z = 1 };
};

template<class T_type>
class foo<T_type, 2> {
public:
  enum bar { z = 2 };
};

int main()
{
    if ((foo<int,3>::z == 0) && (foo<double,3>::z == 1) 
       && (foo<float,2>::z == 2))
           return 0;
    else
        return 1;
}
