// { dg-do assemble  }

class OBJECT
{
    int	a;
};

 

template <class T> class TESTA
{
public:
    TESTA();
      T	foo(int i) {T t = 0; return t;}
};



void foo()
{
    TESTA<OBJECT *>   *foo;

    foo = new TESTA<OBJECT *>;
}

