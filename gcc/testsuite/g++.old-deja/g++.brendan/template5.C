// { dg-do assemble  }
// GROUPS passed templates
template<class T>
class Vector
{
public:
    Vector(int x);
    ~Vector();
    T& operator [] (int i);

private:
    T* v;
    int sz;
} ;

template<class T>
Vector<T>::Vector(int x)
{
    sz = x;
    v = new T (sz);
} 

template<class T>
Vector<T>::~Vector()
    { delete [] v; } 

template<class T>
T &
Vector<T>::operator [] (int i)
    { return v[i]; } 

int
main(int, char **)
{
    Vector<int> intvec(3);

    intvec[0] = 1;
    intvec[1] = 2;
    intvec[2] = 3;

    for (int i = 0; i < 3; i++)
	intvec[i];

    return 0;
}



