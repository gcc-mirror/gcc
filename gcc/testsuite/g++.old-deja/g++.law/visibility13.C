// { dg-do assemble  }
// GROUPS passed visibility
// visibility file
// From: dinh@cs.ucla.edu (Dinh Le)
// Date:     Mon, 12 Jul 93 22:21:06 -0700
// Subject:  class, template and their scoping problem
// Message-ID: <9307130521.AA18312@oahu.cs.ucla.edu>

#include <iostream>
#include <cassert>

//     ---------------   Array.h  &&  Array.cc   ------------------

using namespace std;

const int ArraySize = 12;

template <class> class Array_RC;

template <class Type>
class Array {
  friend class Array_RC<Type>;
public:
    Array(const Type *ar, int sz) { init(ar,sz); }
    virtual ~Array() { delete [] ia; }
    virtual void print(ostream& = cout);
    virtual Type& operator[](int ix) { return ia[ix]; }
private:
    void init(const Type*, int);
    int size;
    int *ia;
};

template <class Type>
ostream& operator<<( ostream& os, Array<Type>& ar )
{
    ar.print(os);
    return os;
}

template <class Type>
void Array<Type>::print(ostream& os)
{
    const int lineLength = 12;

    os << "( " << size << " )< ";
    for (int ix = 0; ix < size; ++ix) {
        if (ix % lineLength == 0 && ix) os << "\n\t";
        os << ia[ ix ];

        if (ix % lineLength != lineLength-1 &&
            ix != size-1)
            os << ", ";
    }
    os << " >\n";
}

template <class Type>
void Array<Type>::init(const Type *array, int sz)
{
    ia = new Type[size = sz];

    for (int ix = 0; ix < size; ++ix)
        ia[ix] = (array!=0) ? array[ix] : (Type)0;
}

//     ---------------   Array_RC.h  &&  Array_RC.cc   ----------------

template <class Type>
class Array_RC : public Array<Type> {
public:
    Array_RC(const Type *ar, int sz);
    Type& operator[](int ix);
};

template <class Type>
Array_RC<Type>::Array_RC(const Type *ar, int sz) : Array<Type>(ar, sz) {}

template <class Type>
Type &Array_RC<Type>::operator[](int ix) {
    assert(ix >= 0 && ix < this->size);
    return this->ia[ix];
}

//    -------------------   Test routine   ----------------------

template <class Type>
void try_array( Array<Type> &iA )
{
    cout << "try_array: initial array values:\n";
    cout << iA << endl;
}

template <class Type>
inline void
try_array( Array_RC<Type> &rc )
{
    try_array( ((Array<Type>&)rc) );
}

int main()
{
    static int ia[10] = { 12, 7, 14, 9, 128, 17, 6, 3, 27, 5 };
    Array_RC<int> iA(ia, 10);

    cout << "template Array_RC class" << endl;
    try_array(iA);

    return 0;
}

template class Array_RC<int>;
