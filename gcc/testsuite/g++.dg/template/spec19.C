// PR c++/18962

template<class T1,int N1>
class Class
{
public:
  template <class T2,int N2>
  void function( const Class<T2,N2>& );
};

template<>
template<class T2,int N2>
void Class<int,1>::function( const Class<T2,N2>& param ) 
{
  param; // make sure we use the argument list from the definition.
}

int main()
{
  Class<int,1> instance;
  Class<char,2> param;
  instance.function( param );
}
