// Build don't link: 

class OBJECT
{
    int	a;
};

 

template <class T> class TESTA
{
public:
    TESTA();
      T	foo(int i) {T t = 0; return t};	// ERROR - no semi
};



void foo()
{
    TESTA<OBJECT *>   *foo;

    foo = new TESTA<OBJECT *>;
}
