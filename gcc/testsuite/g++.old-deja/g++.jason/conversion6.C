// { dg-do run  }
// PRMS Id: g++/6034

extern "C" int printf (const char *, ...);

class Base
{
	char x;
};

template <class T>
// remove the public Base inheritance and the problem goes away...
class Container : public Base
{
public:

    Container(const T& aValue): myValue(aValue) { }
    
    operator const T&(void) const
    {
	printf("Container::const T& called\n");
	return myValue;
    }
    
protected:

    T myValue;
};

typedef unsigned short Type;

typedef Container<Type> TypeContainer;

int main(void)
{
    TypeContainer myTypeContainer(2);
    Type t = myTypeContainer;

    printf ("myType = %d\n", t);
    return t != 2;
}
