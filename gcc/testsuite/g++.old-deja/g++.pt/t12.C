// Build don't link: 

class OBJECT {int a;};
class STDFILE {int b;};

template <class T> class VECTOR {
    T *v;
    int sz;
public:
    T& elem(int i) { return v[i]; }
    T& operator[] (int i);
};

template <class T>
class PVECTOR : VECTOR<void *> {
public:
    T*& elem(int i)
        {return (T*&) VECTOR<void *>::elem(i); }
    T*& operator[] (int i)
        {return (T*&) VECTOR<void *>::operator[](i);}
};

PVECTOR<OBJECT *> *foo;

PVECTOR<STDFILE *> *goo;
