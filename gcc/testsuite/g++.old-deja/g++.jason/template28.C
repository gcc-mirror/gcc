// PRMS Id: 7179

template <class T>
class Car{
public:
   Car();
} ;

class Wheels{
public:
   Wheels();
} ;

class Shop
{
public:
   Shop();
private:
   Car<Wheels> car ;
} ;

Wheels::Wheels() {}

Shop::Shop() {}

int main()
{
   Shop shop ;
   return 0 ;
}

template <class T>
Car<T>::Car() {}
