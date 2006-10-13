// PR c++/18962

template<class T1,int N1>
class Klasse
{
public:
  template <class T2,int N2>
  void function( const Klasse<T2,N2>& );
};

template<>
template<class T2,int N2>
void Klasse<int,1>::function( const Klasse<T2,N2>& param ) 
{
  param; // make sure we use the argument list from the definition.
}

int main()
{
  Klasse<int,1> instance;
  Klasse<char,2> param;
  instance.function( param );
}
