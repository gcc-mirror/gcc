// Build don't link: 
// GROUPS passed scoping
// This is fixed when NEW_CLASS_SCOPING is on.

template<class T>
class ArrayG {
public:
    ArrayG();
protected:
    const unsigned INITIAL;
    T* array;			 
};

template<class T>
ArrayG<T>::ArrayG():
array(new T[INITIAL])
{ }

struct X {
    struct Correspondence {
	int i;
    };

    void fill(ArrayG<Correspondence>& a);
};
